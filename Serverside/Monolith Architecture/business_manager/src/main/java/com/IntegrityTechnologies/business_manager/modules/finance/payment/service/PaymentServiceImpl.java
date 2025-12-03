package com.IntegrityTechnologies.business_manager.modules.finance.payment.service;

import com.IntegrityTechnologies.business_manager.common.TxnCodeGenerator;
import com.IntegrityTechnologies.business_manager.config.OptimisticRetryRunner;
import com.IntegrityTechnologies.business_manager.modules.finance.accounts.service.AccountingService;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.model.SaleLineItem;
import com.IntegrityTechnologies.business_manager.modules.person.entity.customer.service.CustomerService;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.model.Sale;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.repository.SaleRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.dto.PaymentRequest;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.dto.PaymentResponse;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.model.Payment;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.repository.PaymentRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.service.InventoryService;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.*;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.*;

@Service
@RequiredArgsConstructor
public class PaymentServiceImpl implements PaymentService {

    private final PaymentRepository paymentRepository;
    private final SaleRepository saleRepository;
    private final AccountingService accountingService;
    private final CustomerService customerService;
    private final InventoryService inventoryService;

    @Override
    @Transactional
    public PaymentResponse processPayment(PaymentRequest req) {

        Sale sale = saleRepository.findById(req.getSaleId())
                .orElseThrow(() -> new IllegalArgumentException("Sale not found: " + req.getSaleId()));

        String txnCode;

        if ("MPESA".equalsIgnoreCase(req.getMethod()) && req.getProviderReference() != null) {
            txnCode = req.getProviderReference();
        } else {
            txnCode = TxnCodeGenerator.generate();
        }

        Payment p = new Payment();
        p.setSale(sale);
        p.setAmount(req.getAmount());
        p.setMethod(req.getMethod());
        p.setProviderReference(req.getProviderReference());
        p.setStatus("PENDING");
        p.setTimestamp(LocalDateTime.now());
        p.setNote(req.getNote());
        p.setTransactionCode(txnCode);

        Payment saved = paymentRepository.save(p);

        // CASH & MPESA with reference = SUCCESS instantly
        boolean isAutoClear = "CASH".equalsIgnoreCase(req.getMethod()) ||
                ("MPESA".equalsIgnoreCase(req.getMethod()) && req.getProviderReference() != null);

        if (isAutoClear) {
            saved.setStatus("SUCCESS");
            paymentRepository.save(saved);

            // inside PaymentServiceImpl.processPayment (after payment is marked SUCCESS)
            if (saved.getStatus().equals("SUCCESS")) {
                for (SaleLineItem li : sale.getLineItems()) {
                    inventoryService.releaseReservationVariant(
                            li.getProductVariantId(),
                            li.getBranchId(),
                            li.getQuantity(),
                            "PAYMENT:" + saved.getId()
                    );
                    inventoryService.decrementVariantStock(
                            li.getProductVariantId(),
                            li.getBranchId(),
                            li.getQuantity(),
                            "PAYMENT:" + saved.getId()
                    );
                }
            }

            String username = getCurrentUsername();

            OptimisticRetryRunner.runWithRetry(() -> {
                accountingService.recordPayment(
                        saved.getId(),
                        sale.getId(),
                        saved.getAmount(),
                        saved.getMethod(),
                        saved.getProviderReference(),
                        username,
                        txnCode
                );
                return null;
            });
        }

        // Update Sale Status
        sale.getPayments().add(saved);
        BigDecimal totalPaid = sale.getPayments().stream()
                .map(Payment::getAmount)
                .reduce(BigDecimal.ZERO, BigDecimal::add);

        if (totalPaid.compareTo(sale.getTotalAmount()) >= 0) {
            sale.setStatus(Sale.SaleStatus.COMPLETED);
        }

        saleRepository.save(sale);

        try {
            if (sale.getCustomerId() != null) {
                customerService.recordPayment(sale.getCustomerId(), saved.getId(), saved.getAmount(), saved.getTimestamp());
            }
        } catch (Exception ex) {
            // don't fail payment on customer-recording problems
            // log (use your logger)
            System.err.println("Failed to record payment to customer: " + ex.getMessage());
        }

        return toResponse(saved);
    }

    @Override
    public PaymentResponse getPayment(UUID paymentId) {
        Payment p = paymentRepository.findById(paymentId)
                .orElseThrow(() -> new IllegalArgumentException("Payment not found: " + paymentId));
        return toResponse(p);
    }

    @Override
    @Transactional(readOnly = true)
    public Page<PaymentResponse> listPayments(int page, int size, String method, String status, UUID saleId) {
        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.DESC, "timestamp"));
        // build Example or simple repo query - fall back to filter in-memory if needed
        Page<Payment> pageResult = paymentRepository.findAll(pageable);

        // filter
        List<PaymentResponse> mapped = pageResult.stream()
                .filter(p -> method == null || method.equalsIgnoreCase(p.getMethod()))
                .filter(p -> status == null || status.equalsIgnoreCase(p.getStatus()))
                .filter(p -> saleId == null || (p.getSale() != null && saleId.equals(p.getSale().getId())))
                .map(this::toResponse)
                .toList();

        return new PageImpl<>(mapped, pageable, mapped.size());
    }

    @Override
    @Transactional
    public PaymentResponse refundPayment(UUID paymentId) {
        Payment p = paymentRepository.findById(paymentId)
                .orElseThrow(() -> new IllegalArgumentException("Payment not found: " + paymentId));

        if (!"SUCCESS".equalsIgnoreCase(p.getStatus())) {
            throw new IllegalStateException("Only successful payments can be refunded");
        }

        // simple refund flow: create reversing journal via accounting service and mark payment refunded
        String username = getCurrentUsername();
        OptimisticRetryRunner.runWithRetry(() -> {
            accountingService.recordRefund(p.getId(), p.getSale().getId(), p.getAmount(), p.getMethod(), username);
            return null;
        });

        p.setStatus("REFUNDED");
        paymentRepository.save(p);

        // adjust sale status
        Sale sale = p.getSale();
        BigDecimal totalPaid = sale.getPayments().stream()
                .filter(pay -> !"REFUNDED".equalsIgnoreCase(pay.getStatus()))
                .map(Payment::getAmount)
                .reduce(BigDecimal.ZERO, BigDecimal::add);
        if (totalPaid.compareTo(Optional.ofNullable(sale.getTotalAmount()).orElse(BigDecimal.ZERO)) < 0) {
            sale.setStatus(Sale.SaleStatus.CREATED);
        }
        saleRepository.save(sale);

        return toResponse(p);
    }

    @Override
    @Transactional(readOnly = true)
    public Object reconcile(String fromIso, String toIso) {
        // basic reconciliation: list payments grouped by method/status between dates
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

    private PaymentResponse toResponse(Payment p) {
        return PaymentResponse.builder()
                .paymentId(p.getId())
                .saleId(p.getSale().getId())
                .amount(p.getAmount())
                .method(p.getMethod())
                .status(p.getStatus())
                .providerReference(p.getProviderReference())
                .note(p.getNote())
                .timestamp(p.getTimestamp())
                .build();
    }

    private String getCurrentUsername() {
        var auth = org.springframework.security.core.context.SecurityContextHolder.getContext().getAuthentication();
        return auth != null ? auth.getName() : "SYSTEM";
    }
}