package com.IntegrityTechnologies.business_manager.modules.finance.payment.base.service;

import com.IntegrityTechnologies.business_manager.config.util.TxnCodeGenerator;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.adapters.AccountingAccounts;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.api.AccountingEvent;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.api.AccountingFacade;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.JournalEntry;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.EntryDirection;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.JournalEntryRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.service.RevenueRecognitionService;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.base.dto.PaymentDTO;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.base.dto.PaymentRequest;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.base.enums.PaymentStatus;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.base.model.Payment;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.base.repository.PaymentRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.base.model.Sale;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.base.model.SaleLineItem;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.base.repository.SaleRepository;
import com.IntegrityTechnologies.business_manager.modules.person.customer.service.CustomerService;
import com.IntegrityTechnologies.business_manager.security.util.BranchTenantGuard;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import jakarta.persistence.EntityNotFoundException;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.*;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDate;
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
    private final JournalEntryRepository journalEntryRepository;
    private final RevenueRecognitionService revenueRecognitionService;
    private final BranchTenantGuard branchTenantGuard;

    private UUID tenantId() {
        return TenantContext.getTenantId();
    }

    @Override
    @Transactional
    public PaymentDTO processPayment(
            UUID branchId,
            PaymentRequest req
    ) {

        branchTenantGuard.validate(branchId);

        if (req == null || req.getSaleId() == null) {
            throw new IllegalArgumentException(
                    "saleId is required"
            );
        }

        Sale sale =
                saleRepository.findByTenantIdAndBranchIdAndId(tenantId(), branchId, req.getSaleId())
                        .orElseThrow(() ->
                                new EntityNotFoundException(
                                        "Sale not found"
                                )
                        );

        validateSaleOwnership(
                sale,
                branchId
        );

        saleRepository.lockForUpdate(sale.getId());

        validatePaymentRequest(
                branchId,
                req
        );

        BigDecimal saleTotal =
                Optional.ofNullable(
                                sale.getTotalAmount()
                        )
                        .orElse(BigDecimal.ZERO);

        BigDecimal alreadyPaid =
                sale.getPayments() == null
                        ? BigDecimal.ZERO
                        : sale.getPayments()
                        .stream()
                        .filter(p ->
                                p.getStatus()
                                        == PaymentStatus.SUCCESS
                        )
                        .map(Payment::getAmount)
                        .reduce(
                                BigDecimal.ZERO,
                                BigDecimal::add
                        );

        BigDecimal paymentAmount =
                Optional.ofNullable(req.getAmount())
                        .orElse(BigDecimal.ZERO)
                        .setScale(2, RoundingMode.HALF_UP);

        if (paymentAmount.compareTo(BigDecimal.ZERO) <= 0) {

            throw new IllegalArgumentException(
                    "Payment amount must be > 0"
            );
        }

        BigDecimal remaining =
                saleTotal
                        .subtract(alreadyPaid)
                        .setScale(2, RoundingMode.HALF_UP);

        if (paymentAmount.compareTo(remaining) > 0) {

            throw new IllegalArgumentException(
                    "Payment exceeds remaining balance"
            );
        }

        String txnCode =
                req.getProviderReference() != null
                        ? req.getProviderReference()
                        : TxnCodeGenerator.generate();

        Payment payment =
                Payment.builder()
                        .tenantId(tenantId())
                        .branchId(branchId)
                        .sale(sale)
                        .amount(paymentAmount)
                        .method(req.getMethod())
                        .providerReference(
                                req.getProviderReference()
                        )
                        .status(PaymentStatus.SUCCESS)
                        .timestamp(LocalDateTime.now())
                        .note(req.getNote())
                        .transactionCode(txnCode)
                        .build();

        payment =
                paymentRepository.save(payment);

        UUID debitAccount =
                switch (payment.getMethod().toUpperCase()) {

                    case "CASH" ->
                            accountingAccounts.get(
                                    tenantId(),
                                    branchId,
                                    "CASH"
                            );

                    case "BANK" ->
                            accountingAccounts.get(
                                    tenantId(),
                                    branchId,
                                    "BANK"
                            );

                    case "MPESA" ->
                            accountingAccounts.get(
                                    tenantId(),
                                    branchId,
                                    "MPESA"
                            );

                    default ->
                            throw new IllegalArgumentException(
                                    "Unsupported payment method"
                            );
                };

        accountingFacade.post(
                AccountingEvent.builder()
                        .eventId(UUID.randomUUID())
                        .sourceModule("PAYMENT")
                        .sourceId(payment.getId())
                        .reference(
                                payment.getTransactionCode()
                        )
                        .description("Payment received")
                        .performedBy(currentUser())
                        .tenantId(tenantId())
                        .branchId(branchId)
                        .accountingDate(LocalDate.now())
                        .entries(
                                List.of(
                                        AccountingEvent.Entry.builder()
                                                .accountId(debitAccount)
                                                .direction(
                                                        EntryDirection.DEBIT
                                                )
                                                .amount(
                                                        payment.getAmount()
                                                )
                                                .build(),

                                        AccountingEvent.Entry.builder()
                                                .accountId(
                                                        accountingAccounts.get(
                                                                tenantId(),
                                                                branchId,
                                                                "ACCOUNTS_RECEIVABLE"
                                                        )
                                                )
                                                .direction(
                                                        EntryDirection.CREDIT
                                                )
                                                .amount(
                                                        payment.getAmount()
                                                )
                                                .build()
                                )
                        )
                        .build()
        );

        revenueRecognitionService
                .recognizeIfEligible(sale);

        if (sale.getPayments() == null) {
            sale.setPayments(new ArrayList<>());
        }

        sale.getPayments().add(payment);

        saleRepository.save(sale);

        if (sale.getCustomerId() != null) {

            try {

                customerService.recordPayment(
                        branchId,
                        sale.getCustomerId(),
                        payment.getId(),
                        payment.getAmount(),
                        payment.getTimestamp()
                );

            } catch (Exception ex) {

                log.warn(
                        "Failed to record customer payment history",
                        ex
                );
            }
        }

        return toDTO(payment);
    }

    @Override
    public PaymentDTO getPayment(
            UUID branchId,
            UUID paymentId
    ) {

        branchTenantGuard.validate(branchId);

        Payment p =
                paymentRepository
                        .findByTenantIdAndBranchIdAndId(
                                tenantId(),
                                branchId,
                                paymentId
                        )
                        .orElseThrow(() ->
                                new EntityNotFoundException(
                                        "Payment not found"
                                )
                        );

        return toDTO(p);
    }

    @Override
    @Transactional(readOnly = true)
    public Page<PaymentDTO> listPayments(
            UUID branchId,
            int page,
            int size,
            String method,
            String status,
            UUID saleId
    ) {

        branchTenantGuard.validate(branchId);

        Pageable pageable =
                PageRequest.of(
                        page,
                        size,
                        Sort.by(
                                Sort.Direction.DESC,
                                "timestamp"
                        )
                );

        Page<Payment> pageRes;

        if (method != null) {

            pageRes =
                    paymentRepository
                            .findByTenantIdAndBranchIdAndMethodIgnoreCase(
                                    tenantId(),
                                    branchId,
                                    method,
                                    pageable
                            );

        } else if (status != null) {

            pageRes =
                    paymentRepository
                            .findByTenantIdAndBranchIdAndStatus(
                                    tenantId(),
                                    branchId,
                                    PaymentStatus.valueOf(status),
                                    pageable
                            );

        } else {

            pageRes =
                    paymentRepository
                            .findByTenantIdAndBranchId(
                                    tenantId(),
                                    branchId,
                                    pageable
                            );
        }

        List<PaymentDTO> mapped =
                pageRes.stream()
                        .filter(p ->
                                saleId == null
                                        || (
                                        p.getSale() != null
                                                && saleId.equals(
                                                p.getSale().getId()
                                        )
                                )
                        )
                        .map(this::toDTO)
                        .toList();

        return new PageImpl<>(
                mapped,
                pageable,
                pageRes.getTotalElements()
        );
    }

    @Override
    @Transactional
    public PaymentDTO refundPayment(
            UUID branchId,
            UUID paymentId
    ) {

        branchTenantGuard.validate(branchId);

        Payment payment =
                paymentRepository
                        .findByTenantIdAndBranchIdAndId(
                                tenantId(),
                                branchId,
                                paymentId
                        )
                        .orElseThrow(() ->
                                new EntityNotFoundException(
                                        "Payment not found"
                                )
                        );

        if (payment.getStatus()
                != PaymentStatus.SUCCESS) {

            throw new IllegalStateException(
                    "Only SUCCESS payments can be refunded"
            );
        }

        JournalEntry originalJournal =
                journalEntryRepository
                        .findByTenantIdAndSourceModuleAndSourceId(
                                tenantId(),
                                "PAYMENT",
                                payment.getId()
                        )
                        .orElseThrow(() ->
                                new IllegalStateException(
                                        "Original payment journal not found"
                                )
                        );

        accountingFacade.reverseJournal(
                originalJournal.getId(),
                "Payment refund",
                currentUser()
        );

        payment.setStatus(
                PaymentStatus.REFUNDED
        );

        paymentRepository.save(payment);

        return toDTO(payment);
    }

    @Override
    @Transactional
    public PaymentDTO reversePayment(
            UUID branchId,
            UUID paymentId,
            String note
    ) {

        branchTenantGuard.validate(branchId);

        Payment payment =
                paymentRepository
                        .findByTenantIdAndBranchIdAndId(
                                tenantId(),
                                branchId,
                                paymentId
                        )
                        .orElseThrow(() ->
                                new EntityNotFoundException(
                                        "Payment not found"
                                )
                        );

        if (payment.getStatus()
                != PaymentStatus.SUCCESS) {

            throw new IllegalStateException(
                    "Only SUCCESS payments can be reversed"
            );
        }

        JournalEntry originalJournal =
                journalEntryRepository
                        .findByTenantIdAndSourceModuleAndSourceId(
                                tenantId(),
                                "PAYMENT",
                                payment.getId()
                        )
                        .orElseThrow(() ->
                                new IllegalStateException(
                                        "Original payment journal not found"
                                )
                        );

        accountingFacade.reverseJournal(
                originalJournal.getId(),
                note != null
                        ? note
                        : "Payment reversal",
                currentUser()
        );

        payment.setStatus(
                PaymentStatus.REVERSED
        );

        payment.setNote(
                note != null
                        ? note
                        : "Payment reversed"
        );

        paymentRepository.save(payment);

        return toDTO(payment);
    }

    @Override
    @Transactional(readOnly = true)
    public Object reconcile(
            UUID branchId,
            String fromIso,
            String toIso
    ) {

        branchTenantGuard.validate(branchId);

        LocalDateTime from =
                LocalDateTime.parse(
                        fromIso + "T00:00:00"
                );

        LocalDateTime to =
                LocalDateTime.parse(
                        toIso + "T23:59:59"
                );

        List<Payment> payments =
                paymentRepository
                        .findByTenantIdAndBranchIdAndTimestampBetween(
                                tenantId(),
                                branchId,
                                from,
                                to
                        );

        Map<String, Object> report =
                new HashMap<>();

        report.put(
                "totalPayments",
                payments.size()
        );

        report.put(
                "totalAmount",
                payments.stream()
                        .filter(p ->
                                p.getStatus() == PaymentStatus.SUCCESS
                        )
                        .map(Payment::getAmount)
                        .reduce(
                                BigDecimal.ZERO,
                                BigDecimal::add
                        )
        );

        return report;
    }

    @Override
    @Transactional(
            readOnly = true,
            noRollbackFor = IllegalArgumentException.class
    )
    public void validatePaymentRequest(
            UUID branchId,
            PaymentRequest req
    ) {

        branchTenantGuard.validate(branchId);

        if (req == null
                || req.getProviderReference() == null) {
            return;
        }

        var existing =
                paymentRepository
                        .findByTenantIdAndBranchIdAndProviderReference(
                                tenantId(),
                                branchId,
                                req.getProviderReference()
                        );

        if (existing.isPresent()) {

            Payment p = existing.get();

            if (req.getSaleId() == null
                    || !p.getSale()
                    .getId()
                    .equals(req.getSaleId())) {

                throw new IllegalArgumentException(
                        "Provider reference already used"
                );
            }
        }
    }

    @Override
    @Transactional(readOnly = true)
    public BigDecimal totalSuccessfulPaymentsForSale(
            UUID branchId,
            UUID saleId
    ) {

        branchTenantGuard.validate(branchId);

        Sale sale =
                saleRepository.findByTenantIdAndBranchIdAndId(tenantId(), branchId, saleId)
                        .orElseThrow(() ->
                                new EntityNotFoundException(
                                        "Sale not found"
                                )
                        );

        validateSaleOwnership(
                sale,
                branchId
        );

        if (sale.getPayments() == null) {
            return BigDecimal.ZERO;
        }

        return sale.getPayments()
                .stream()
                .filter(p ->
                        p.getStatus()
                                == PaymentStatus.SUCCESS
                )
                .map(Payment::getAmount)
                .reduce(
                        BigDecimal.ZERO,
                        BigDecimal::add
                );
    }

    private void validateSaleOwnership(
            Sale sale,
            UUID branchId
    ) {

        boolean belongs =
                sale.getLineItems()
                        .stream()
                        .map(SaleLineItem::getBranchId)
                        .allMatch(branchId::equals);

        if (!belongs) {

            throw new SecurityException(
                    "Sale does not belong to branch"
            );
        }
    }

    private String currentUser() {

        var auth =
                SecurityContextHolder
                        .getContext()
                        .getAuthentication();

        return auth != null
                ? auth.getName()
                : "SYSTEM";
    }

    private PaymentDTO toDTO(
            Payment p
    ) {

        return PaymentDTO.builder()
                .paymentId(p.getId())
                .amount(p.getAmount())
                .method(p.getMethod())
                .status(p.getStatus())
                .providerReference(
                        p.getProviderReference()
                )
                .note(p.getNote())
                .timestamp(p.getTimestamp())
                .transactionCode(
                        p.getTransactionCode()
                )
                .build();
    }
}