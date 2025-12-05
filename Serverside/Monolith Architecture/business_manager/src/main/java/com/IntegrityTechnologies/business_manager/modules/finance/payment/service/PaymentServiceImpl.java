package com.IntegrityTechnologies.business_manager.modules.finance.payment.service;

import com.IntegrityTechnologies.business_manager.common.TxnCodeGenerator;
import com.IntegrityTechnologies.business_manager.config.OptimisticRetryRunner;
import com.IntegrityTechnologies.business_manager.modules.finance.accounts.service.AccountingService;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.dto.PaymentDTO;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.dto.PaymentRequest;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.dto.PaymentDTO;
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
    private final AccountingService accountingService;
    private final CustomerService customerService;
    private final InventoryService inventoryService;

    @Override
    @Transactional
    public PaymentDTO processPayment(PaymentRequest req) {
        log.info("Processing payment for sale={}, amount={}, method={}", req.getSaleId(), req.getAmount(), req.getMethod());

        if (req == null || req.getSaleId() == null) throw new IllegalArgumentException("saleId is required");

        // 1) load sale
        Sale sale = saleRepository.findById(req.getSaleId())
                .orElseThrow(() -> new IllegalArgumentException("Sale not found: " + req.getSaleId()));
        saleRepository.lockForUpdate(sale.getId());

        BigDecimal saleTotal = Optional.ofNullable(sale.getTotalAmount()).orElse(BigDecimal.ZERO);
        BigDecimal alreadyPaid = Optional.ofNullable(sale.getPayments())
                .orElse(Collections.emptyList())
                .stream()
                .map(Payment::getAmount)
                .reduce(BigDecimal.ZERO, BigDecimal::add);

        BigDecimal paymentAmount = Optional.ofNullable(req.getAmount()).orElse(BigDecimal.ZERO);

        // 0) basic validation
        if (paymentAmount.compareTo(BigDecimal.ZERO) <= 0) {
            throw new IllegalArgumentException("Payment amount must be > 0");
        }

        // remaining balance (sale total - already paid)
        BigDecimal remaining = saleTotal.subtract(alreadyPaid);

        // Disallow payments when nothing is due
        if (remaining.compareTo(BigDecimal.ZERO) <= 0) {
            throw new IllegalStateException("Sale is already fully paid. No payment is accepted.");
        }

        // Disallow overpayment (strict)
        if (paymentAmount.compareTo(remaining) > 0) {
            throw new IllegalArgumentException(String.format(
                    "Payment exceeds remaining balance. Remaining: %s, Attempted: %s",
                    remaining.toPlainString(), paymentAmount.toPlainString()));
        }

        BigDecimal totalAfter = alreadyPaid.add(paymentAmount);
        boolean willBeFullyPaid = totalAfter.compareTo(saleTotal) >= 0;

        // 2) transaction code (mpesa with providerRef uses providerRef)
        String txnCode = ("MPESA".equalsIgnoreCase(req.getMethod()) && req.getProviderReference() != null)
                ? req.getProviderReference()
                : TxnCodeGenerator.generate();

        // 3) create Payment entity
        Payment p = new Payment();
        p.setSale(sale);
        p.setAmount(paymentAmount);
        p.setMethod(req.getMethod());
        p.setProviderReference(req.getProviderReference());
        p.setStatus("PENDING");
        p.setTimestamp(LocalDateTime.now());
        p.setNote(req.getNote());
        p.setTransactionCode(txnCode);

        // 4) persist payment and operate on managed instance
        Payment saved = paymentRepository.save(p);
        log.info("Payment persisted (id={}, status={})", saved.getId(), saved.getStatus());

        boolean autoClear = "CASH".equalsIgnoreCase(req.getMethod())
                || ("MPESA".equalsIgnoreCase(req.getMethod()) && req.getProviderReference() != null);

        if (autoClear) {
            saved.setStatus("SUCCESS");
            saved = paymentRepository.save(saved);
            log.info("Payment auto-cleared (id={})", saved.getId());

            // Only execute stock + accounting when sale becomes fully paid
            if (willBeFullyPaid) {
                log.info("Sale {} becomes fully paid; releasing reservations + decrementing stock", sale.getId());

                for (SaleLineItem li : Optional.ofNullable(sale.getLineItems()).orElse(Collections.emptyList())) {
                    // release reservation (best-effort)
                    try {
                        inventoryService.releaseReservationVariant(
                                li.getProductVariantId(),
                                li.getBranchId(),
                                li.getQuantity(),
                                "PAYMENT:" + saved.getId()
                        );
                    } catch (Exception ex) {
                        log.warn("Failed to release reservation variant={} branch={} : {}", li.getProductVariantId(), li.getBranchId(), ex.getMessage());
                    }

                    // decrement stock (critical)
                    try {
                        inventoryService.decrementVariantStock(
                                li.getProductVariantId(),
                                li.getBranchId(),
                                li.getQuantity(),
                                "PAYMENT:" + saved.getId()
                        );
                    } catch (Exception ex) {
                        log.error("Failed to decrement stock variant={} branch={} : {}", li.getProductVariantId(), li.getBranchId(), ex.getMessage());
                        // this is critical — fail the transaction so the system doesn't get inconsistent
                        throw ex;
                    }
                }

                // accounting (retry-safe)
                final UUID finalPaymentId = saved.getId();
                final UUID finalSaleId = sale.getId();
                final BigDecimal finalAmount = saved.getAmount();
                final String finalMethod = saved.getMethod();
                final String finalProviderRef = saved.getProviderReference();
                final String finalUsername = getCurrentUsername();
                final String finalTxnCode = txnCode;

                try {
                    OptimisticRetryRunner.runWithRetry(() -> {
                        accountingService.recordPayment(
                                finalPaymentId,
                                finalSaleId,
                                finalAmount,
                                finalMethod,
                                finalProviderRef,
                                finalUsername,
                                finalTxnCode
                        );
                        return null;
                    });
                } catch (Exception ex) {
                    log.error("Accounting recording failed for payment {}: {}", saved.getId(), ex.getMessage());
                    // make decision: throw so transaction rolls back (preferred)
                    throw ex;
                }
            } else {
                log.info("Payment {} is partial for sale {}; not moving stock/accounting yet", saved.getId(), sale.getId());
            }
        }

        // 5) attach managed payment to sale and update status
        if (sale.getPayments() == null) sale.setPayments(new ArrayList<>());
        sale.getPayments().add(saved);

        BigDecimal totalPaidNow = sale.getPayments().stream().map(Payment::getAmount).reduce(BigDecimal.ZERO, BigDecimal::add);
        if (totalPaidNow.compareTo(saleTotal) >= 0) {
            sale.setStatus(Sale.SaleStatus.COMPLETED);
        }
        saleRepository.save(sale);
        log.info("Sale {} updated status={} totalPaid={}", sale.getId(), sale.getStatus(), totalPaidNow);

        // 6) best-effort: record customer ledger
        try {
            if (sale.getCustomerId() != null) {
                customerService.recordPayment(sale.getCustomerId(), saved.getId(), saved.getAmount(), saved.getTimestamp());
            }
        } catch (Exception ex) {
            log.error("Failed to update customer ledger for sale={} payment={}: {}", sale.getId(), saved.getId(), ex.getMessage());
        }

        // 7) return DTO
        return toDTO(saved);
    }

    @Override
    public PaymentDTO getPayment(UUID paymentId) {
        Payment p = paymentRepository.findById(paymentId)
                .orElseThrow(() -> new IllegalArgumentException("Payment not found: " + paymentId));
        return toDTO(p);
    }

    @Override
    @Transactional(readOnly = true)
    public Page<PaymentDTO> listPayments(int page, int size, String method, String status, UUID saleId) {
        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.DESC, "timestamp"));
        Page<Payment> pageResult = paymentRepository.findAll(pageable);
        List<PaymentDTO> mapped = pageResult.stream()
                .filter(p -> method == null || method.equalsIgnoreCase(p.getMethod()))
                .filter(p -> status == null || status.equalsIgnoreCase(p.getStatus()))
                .filter(p -> saleId == null || (p.getSale() != null && saleId.equals(p.getSale().getId())))
                .map(this::toDTO)
                .toList();
        return new PageImpl<>(mapped, pageable, mapped.size());
    }

    @Override
    @Transactional
    public PaymentDTO refundPayment(UUID paymentId) {
        Payment p = paymentRepository.findById(paymentId)
                .orElseThrow(() -> new IllegalArgumentException("Payment not found: " + paymentId));

        if (!"SUCCESS".equalsIgnoreCase(p.getStatus())) {
            throw new IllegalStateException("Only successful payments can be refunded");
        }

        String username = getCurrentUsername();
        OptimisticRetryRunner.runWithRetry(() -> {
            accountingService.recordRefund(p.getId(), p.getSale().getId(), p.getAmount(), p.getMethod(), username);
            return null;
        });

        p.setStatus("REFUNDED");
        paymentRepository.save(p);

        Sale sale = p.getSale();
        BigDecimal totalPaid = sale.getPayments().stream()
                .filter(pay -> !"REFUNDED".equalsIgnoreCase(pay.getStatus()))
                .map(Payment::getAmount)
                .reduce(BigDecimal.ZERO, BigDecimal::add);

        if (totalPaid.compareTo(Optional.ofNullable(sale.getTotalAmount()).orElse(BigDecimal.ZERO)) < 0) {
            sale.setStatus(Sale.SaleStatus.CREATED);
        }
        saleRepository.save(sale);

        return toDTO(p);
    }

    @Transactional
    public PaymentDTO reversePayment(UUID paymentId, String note) {
        log.info("Initiating reversal for payment {}", paymentId);

        Payment p = paymentRepository.findById(paymentId)
                .orElseThrow(() -> new IllegalArgumentException("Payment not found: " + paymentId));

        if (!"SUCCESS".equalsIgnoreCase(p.getStatus())) {
            throw new IllegalStateException("Only SUCCESS payments can be reversed");
        }

        Sale sale = p.getSale();
        BigDecimal amount = p.getAmount();
        LocalDateTime ts = LocalDateTime.now();
        String username = getCurrentUsername();

        // 1) reverse accounting (retry-safe)
        final UUID finalPaymentId = p.getId();
        final UUID finalSaleId = sale.getId();
        final BigDecimal finalAmount = amount;
        try {
            OptimisticRetryRunner.runWithRetry(() -> {
                accountingService.reverseSalePayment(
                        finalSaleId,
                        finalAmount,
                        "REVERSAL:" + finalPaymentId
                );
                return null;
            });
        } catch (Exception ex) {
            log.error("Accounting reversal failed for payment {}: {}", finalPaymentId, ex.getMessage());
            throw ex;
        }

        // 2) customer ledger (best-effort)
        try {
            if (sale.getCustomerId() != null) {
                customerService.recordRefund(sale.getCustomerId(), p.getId(), amount, note != null ? note : "Payment reversed");
            }
        } catch (Exception ex) {
            log.warn("Customer ledger reversal failed for sale={}, payment={}: {}", sale.getId(), p.getId(), ex.getMessage());
        }

        // 3) mark payment refunded
        p.setStatus("REFUNDED");
        p.setNote(note != null ? note : "Payment reversed");
        paymentRepository.save(p);

        // 4) update sale status if needed
        BigDecimal remaining = sale.getPayments().stream()
                .filter(pp -> !"REFUNDED".equalsIgnoreCase(pp.getStatus()))
                .map(Payment::getAmount)
                .reduce(BigDecimal.ZERO, BigDecimal::add);

        if (remaining.compareTo(sale.getTotalAmount()) < 0) {
            sale.setStatus(Sale.SaleStatus.CREATED);
            saleRepository.save(sale);
        }

        log.info("Payment reversal complete for {}, remaining balance={}", paymentId, remaining);
        return toDTO(p);
    }

    @Override
    @Transactional(readOnly = true)
    public Object reconcile(String fromIso, String toIso) {
        LocalDateTime from = LocalDateTime.parse(fromIso + "T00:00:00");
        LocalDateTime to = LocalDateTime.parse(toIso + "T23:59:59");
        List<Payment> payments = paymentRepository.findByTimestampBetween(from, to);

        Map<String, Object> report = new HashMap<>();
        report.put("totalPayments", payments.size());
        BigDecimal totalAmount = payments.stream().map(Payment::getAmount).reduce(BigDecimal.ZERO, BigDecimal::add);
        report.put("totalAmount", totalAmount);

        Map<String, BigDecimal> byMethod = new HashMap<>();
        payments.forEach(p -> byMethod.put(p.getMethod(), byMethod.getOrDefault(p.getMethod(), BigDecimal.ZERO).add(Optional.ofNullable(p.getAmount()).orElse(BigDecimal.ZERO))));
        report.put("byMethod", byMethod);

        Map<String, Long> byStatus = new HashMap<>();
        payments.forEach(p -> byStatus.put(p.getStatus(), byStatus.getOrDefault(p.getStatus(), 0L) + 1));
        report.put("byStatus", byStatus);

        return report;
    }

    private String getCurrentUsername() {
        var auth = SecurityContextHolder.getContext().getAuthentication();
        return auth != null ? auth.getName() : "SYSTEM";
    }

    public PaymentDTO toDTO(Payment p) {
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