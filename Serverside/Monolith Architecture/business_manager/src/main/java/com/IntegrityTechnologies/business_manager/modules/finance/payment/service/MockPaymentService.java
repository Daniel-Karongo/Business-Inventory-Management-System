package com.IntegrityTechnologies.business_manager.modules.finance.payment.service;

import com.IntegrityTechnologies.business_manager.modules.finance.payment.dto.PaymentRequest;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.dto.PaymentResponse;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.model.Payment;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.repository.PaymentRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.repository.SaleRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class MockPaymentService implements PaymentService {

    private final PaymentRepository paymentRepository;
    private final SaleRepository saleRepository;

    @Override
    @Transactional
    public PaymentResponse processPayment(PaymentRequest req) {
        // mock success always
        Payment p = Payment.builder()
                .id(UUID.randomUUID())
                .sale(saleRepository.findById(req.getSaleId()).get())
                .amount(req.getAmount())
                .method(req.getMethod())
                .providerReference(req.getProviderReference())
                .status("SUCCESS")
                .timestamp(LocalDateTime.now())
                .note(req.getNote())
                .build();

        Payment saved = paymentRepository.save(p);

        return PaymentResponse.builder()
                .paymentId(saved.getId())
                .saleId(p.getSale().getId())
                .amount(saved.getAmount())
                .method(saved.getMethod())
                .status(saved.getStatus())
                .providerReference(saved.getProviderReference())
                .note(saved.getNote())
                .timestamp(saved.getTimestamp())
                .build();
    }

    @Override
    public PaymentResponse getPayment(UUID paymentId) {
        Payment p = paymentRepository.findById(paymentId)
                .orElseThrow(() -> new IllegalArgumentException("Payment not found: " + paymentId));

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
}