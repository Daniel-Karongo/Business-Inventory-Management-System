package com.IntegrityTechnologies.business_manager.modules.finance.payment.service;

import com.IntegrityTechnologies.business_manager.config.OptimisticRetryRunner;
import com.IntegrityTechnologies.business_manager.modules.finance.accounts.service.AccountingService;
import com.IntegrityTechnologies.business_manager.modules.person.entity.customer.service.CustomerService;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.model.Sale;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.repository.SaleRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.dto.PaymentRequest;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.dto.PaymentResponse;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.model.Payment;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.repository.PaymentRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class PaymentServiceImpl implements PaymentService {

    private final PaymentRepository paymentRepository;
    private final SaleRepository saleRepository;
    private final AccountingService accountingService;
    private final CustomerService customerService; // optional bean available in your project

    @Override
    @Transactional
    public PaymentResponse processPayment(PaymentRequest req) {
        Sale sale = saleRepository.findById(req.getSaleId())
                .orElseThrow(() -> new IllegalArgumentException("Sale not found: " + req.getSaleId()));

        // Idempotency: if providerReference exists, don't duplicate
        if (req.getProviderReference() != null && paymentRepository.findByProviderReference(req.getProviderReference()).stream().findFirst().isPresent()) {
            // return existing payment
            var existing = paymentRepository.findByProviderReference(req.getProviderReference()).get(0);
            return toResponse(existing);
        }

        Payment p = new Payment();
        p.setSale(sale);
        p.setAmount(req.getAmount());
        p.setMethod(req.getMethod());
        p.setProviderReference(req.getProviderReference());
        p.setStatus("PENDING");
        p.setTimestamp(LocalDateTime.now());
        p.setNote(req.getNote());


        Payment saved = paymentRepository.save(p);

        // If method is CASH, treat as immediate success
        if ("CASH".equalsIgnoreCase(req.getMethod())) {
            saved.setStatus("SUCCESS");
            paymentRepository.save(saved);

            OptimisticRetryRunner.runWithRetry(() -> {
                accountingService.recordPayment(saved.getId(), sale.getId(), saved.getAmount(), saved.getMethod(), saved.getProviderReference(), getCurrentUsername());
                return null;
            });
        } else {
            // For MPESA or others: many flows will mark pending -> then callback will call processPayment or other method
            // For MPESA initiated from MpesaServiceImpl we will call processPayment with providerReference set and treat as success.
            // Here we leave as PENDING if providerReference is null (e.g., initiating checkout)
            if (req.getProviderReference() != null) {
                saved.setStatus("SUCCESS");
                paymentRepository.save(saved);
                OptimisticRetryRunner.runWithRetry(() -> {
                    accountingService.recordPayment(saved.getId(), sale.getId(), saved.getAmount(), saved.getMethod(), saved.getProviderReference(), getCurrentUsername());
                    return null;
                });

            }
        }

        // attach to sale and update sale status based on total paid
        sale.getPayments().add(saved);
        BigDecimal totalPaid = sale.getPayments().stream()
                .filter(pay -> pay.getAmount() != null)
                .map(Payment::getAmount)
                .reduce(BigDecimal.ZERO, BigDecimal::add);

        if (totalPaid.compareTo(sale.getTotalAmount() == null ? BigDecimal.ZERO : sale.getTotalAmount()) >= 0) {
            sale.setStatus(Sale.SaleStatus.COMPLETED);
        } else if (totalPaid.compareTo(BigDecimal.ZERO) > 0) {
            sale.setStatus(Sale.SaleStatus.CREATED); // or PARTIALLY_PAID if you add an enum
        }
        saleRepository.save(sale);

        // record payment against customer (if present)
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
        Payment p = paymentRepository.findById(paymentId).orElseThrow(() -> new IllegalArgumentException("Payment not found: " + paymentId));
        return toResponse(p);
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