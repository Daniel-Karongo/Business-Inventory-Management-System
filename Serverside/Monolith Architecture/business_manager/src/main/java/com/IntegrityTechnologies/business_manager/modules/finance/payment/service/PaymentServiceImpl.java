package com.IntegrityTechnologies.business_manager.modules.finance.payment.service;

import com.IntegrityTechnologies.business_manager.common.TxnCodeGenerator;
import com.IntegrityTechnologies.business_manager.config.OptimisticRetryRunner;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.adapters.AccountingAccounts;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.api.AccountingEvent;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.api.AccountingFacade;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.JournalEntry;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.EntryDirection;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.JournalEntryRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.dto.PaymentDTO;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.dto.PaymentRequest;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.model.Payment;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.repository.PaymentRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.model.Sale;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.model.SaleLineItem;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.repository.SaleRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.customer.service.CustomerService;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.service.InventoryService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.*;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.*;

@Service
@RequiredArgsConstructor
@Slf4j
public class PaymentServiceImpl implements PaymentService {

    private final PaymentRepository paymentRepository;
    private final SaleRepository saleRepository;
    private final AccountingFacade accountingFacade;
    private final AccountingAccounts accountingAccounts;
    private final CustomerService customerService;
    private final InventoryService inventoryService;
    private final JournalEntryRepository journalEntryRepository;

    /* ============================================================
       PROCESS PAYMENT
       ============================================================ */
    @Override
    @Transactional
    public PaymentDTO processPayment(PaymentRequest req) {

        if (req == null || req.getSaleId() == null) {
            throw new IllegalArgumentException("saleId is required");
        }

        Sale sale = saleRepository.findById(req.getSaleId())
                .orElseThrow(() -> new IllegalArgumentException("Sale not found: " + req.getSaleId()));

        // 🔐 lock sale row
        saleRepository.lockForUpdate(sale.getId());

        BigDecimal saleTotal =
                Optional.ofNullable(sale.getTotalAmount()).orElse(BigDecimal.ZERO);

        BigDecimal alreadyPaid =
                sale.getPayments() == null
                        ? BigDecimal.ZERO
                        : sale.getPayments().stream()
                        .filter(p -> "SUCCESS".equalsIgnoreCase(p.getStatus()))
                        .map(Payment::getAmount)
                        .reduce(BigDecimal.ZERO, BigDecimal::add);

        BigDecimal paymentAmount =
                Optional.ofNullable(req.getAmount()).orElse(BigDecimal.ZERO);

        if (paymentAmount.compareTo(BigDecimal.ZERO) <= 0) {
            throw new IllegalArgumentException("Payment amount must be > 0");
        }

        BigDecimal remaining = saleTotal.subtract(alreadyPaid);

        if (remaining.compareTo(BigDecimal.ZERO) <= 0) {
            throw new IllegalStateException("Sale already fully paid");
        }

        if (paymentAmount.compareTo(remaining) > 0) {
            throw new IllegalArgumentException("Payment exceeds remaining balance");
        }

        boolean willBeFullyPaid =
                alreadyPaid.add(paymentAmount).compareTo(saleTotal) >= 0;

        // 🔐 idempotency for provider-based payments (Mpesa)
        if (req.getProviderReference() != null) {
            var existing =
                    paymentRepository.findByProviderReference(req.getProviderReference());

            if (existing != null && !existing.isEmpty()) {
                return toDTO(existing.get(0));
            }
        }

        String txnCode =
                req.getProviderReference() != null
                        ? req.getProviderReference()
                        : TxnCodeGenerator.generate();

        Payment payment = new Payment();
        payment.setSale(sale);
        payment.setAmount(paymentAmount);
        payment.setMethod(req.getMethod());
        payment.setProviderReference(req.getProviderReference());
        payment.setStatus("PENDING");
        payment.setTimestamp(LocalDateTime.now());
        payment.setNote(req.getNote());
        payment.setTransactionCode(txnCode);

        payment = paymentRepository.save(payment);

        boolean autoClear =
                "CASH".equalsIgnoreCase(req.getMethod())
                        || ("MPESA".equalsIgnoreCase(req.getMethod())
                        && req.getProviderReference() != null);

        if (autoClear) {

            if (!"SUCCESS".equalsIgnoreCase(payment.getStatus())) {
                payment.setStatus("SUCCESS");
                payment = paymentRepository.save(payment);
            }

            // ✅ SINGLE journal per payment
            if (!accountingFacade.isAlreadyPosted("PAYMENT", payment.getId())) {

                accountingFacade.post(
                        AccountingEvent.builder()
                                .sourceModule("PAYMENT")
                                .sourceId(payment.getId())
                                .reference(payment.getTransactionCode())
                                .description("Payment received for sale " + sale.getId())
                                .performedBy(currentUser())
                                .entries(List.of(
                                        AccountingEvent.Entry.builder()
                                                .accountId(
                                                        "CASH".equalsIgnoreCase(payment.getMethod())
                                                                ? accountingAccounts.cash()
                                                                : accountingAccounts.bank()
                                                )
                                                .direction(EntryDirection.DEBIT)
                                                .amount(payment.getAmount())
                                                .build(),
                                        AccountingEvent.Entry.builder()
                                                .accountId(accountingAccounts.revenue())
                                                .direction(EntryDirection.CREDIT)
                                                .amount(payment.getAmount())
                                                .build()
                                ))
                                .build()
                );
            }
        }

        sale.getPayments().add(payment);

        if (sale.getPayments().stream()
                .filter(p -> "SUCCESS".equalsIgnoreCase(p.getStatus()))
                .map(Payment::getAmount)
                .reduce(BigDecimal.ZERO, BigDecimal::add)
                .compareTo(saleTotal) >= 0) {

            sale.setStatus(Sale.SaleStatus.COMPLETED);
        }

        saleRepository.save(sale);

        // 👤 customer payment history (unchanged)
        try {
            if (sale.getCustomerId() != null) {
                customerService.recordPayment(
                        sale.getCustomerId(),
                        payment.getId(),
                        payment.getAmount(),
                        payment.getTimestamp()
                );
            }
        } catch (Exception ignored) {
        }

        return toDTO(payment);
    }

    /* ============================================================
       GET PAYMENT
       ============================================================ */
    @Override
    public PaymentDTO getPayment(UUID paymentId) {
        Payment p = paymentRepository.findById(paymentId)
                .orElseThrow(() -> new IllegalArgumentException("Payment not found: " + paymentId));
        return toDTO(p);
    }

    /* ============================================================
       LIST PAYMENTS
       ============================================================ */
    @Override
    @Transactional(readOnly = true)
    public Page<PaymentDTO> listPayments(int page, int size, String method, String status, UUID saleId) {

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.DESC, "timestamp"));
        Page<Payment> pageRes = paymentRepository.findAll(pageable);

        List<PaymentDTO> mapped = pageRes.stream()
                .filter(p -> method == null || method.equalsIgnoreCase(p.getMethod()))
                .filter(p -> status == null || status.equalsIgnoreCase(p.getStatus()))
                .filter(p -> saleId == null || (p.getSale() != null && saleId.equals(p.getSale().getId())))
                .map(this::toDTO)
                .toList();

        return new PageImpl<>(mapped, pageable, mapped.size());
    }

    /* ============================================================
       REFUND PAYMENT
       ============================================================ */
    @Override
    @Transactional
    public PaymentDTO refundPayment(UUID paymentId) {

        Payment payment = paymentRepository.findById(paymentId)
                .orElseThrow(() -> new IllegalArgumentException("Payment not found"));

        if (!"SUCCESS".equalsIgnoreCase(payment.getStatus())) {
            throw new IllegalStateException("Only SUCCESS payments can be refunded");
        }

        String username = currentUser();

        // 🔁 reverse original PAYMENT journal
        JournalEntry originalJournal =
                journalEntryRepository
                        .findBySourceModuleAndSourceId("PAYMENT", payment.getId())
                        .orElseThrow(() ->
                                new IllegalStateException("Original payment journal not found"));

        accountingFacade.reverseJournal(
                originalJournal.getId(),
                "Payment refund",
                username
        );

        payment.setStatus("REFUNDED");
        paymentRepository.save(payment);

        return toDTO(payment);
    }

    /* ============================================================
       REVERSE PAYMENT (ADMIN / CORRECTION)
       ============================================================ */
    @Override
    @Transactional
    public PaymentDTO reversePayment(UUID paymentId, String note) {

        Payment payment = paymentRepository.findById(paymentId)
                .orElseThrow(() -> new IllegalArgumentException("Payment not found"));

        if (!"SUCCESS".equalsIgnoreCase(payment.getStatus())) {
            throw new IllegalStateException("Only SUCCESS payments can be reversed");
        }

        String username = currentUser();

        JournalEntry originalJournal =
                journalEntryRepository
                        .findBySourceModuleAndSourceId("PAYMENT", payment.getId())
                        .orElseThrow(() ->
                                new IllegalStateException("Original payment journal not found"));

        accountingFacade.reverseJournal(
                originalJournal.getId(),
                note != null ? note : "Payment reversal",
                username
        );

        payment.setStatus("REFUNDED");
        payment.setNote(note != null ? note : "Payment reversed");
        paymentRepository.save(payment);

        return toDTO(payment);
    }

    /* ============================================================
       RECONCILIATION
       ============================================================ */
    @Override
    @Transactional(readOnly = true)
    public Object reconcile(String fromIso, String toIso) {

        LocalDateTime from = LocalDateTime.parse(fromIso + "T00:00:00");
        LocalDateTime to = LocalDateTime.parse(toIso + "T23:59:59");

        List<Payment> payments = paymentRepository.findByTimestampBetween(from, to);

        Map<String, Object> report = new HashMap<>();
        report.put("totalPayments", payments.size());
        report.put("totalAmount",
                payments.stream().map(Payment::getAmount).reduce(BigDecimal.ZERO, BigDecimal::add));

        return report;
    }

    /* ============================================================
       HELPERS
       ============================================================ */
    private String currentUser() {
        var auth = SecurityContextHolder.getContext().getAuthentication();
        return auth != null ? auth.getName() : "SYSTEM";
    }

    private PaymentDTO toDTO(Payment p) {
        return PaymentDTO.builder()
                .paymentId(p.getId())
                .amount(p.getAmount())
                .method(p.getMethod())
                .status(p.getStatus())
                .providerReference(p.getProviderReference())
                .note(p.getNote())
                .timestamp(p.getTimestamp())
                .transactionCode(p.getTransactionCode())
                .build();
    }
}