package com.IntegrityTechnologies.business_manager.modules.finance.payment.service;

import com.IntegrityTechnologies.business_manager.modules.finance.payment.dto.PaymentRequest;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.dto.PaymentResponse;
import org.springframework.data.domain.Page;
import org.springframework.transaction.annotation.Transactional;

import java.util.UUID;

public interface PaymentService {
    PaymentResponse processPayment(PaymentRequest request);
    PaymentResponse getPayment(UUID paymentId);

    @Transactional(readOnly = true)
    Page<PaymentResponse> listPayments(int page, int size, String method, String status, UUID saleId);

    @Transactional
    PaymentResponse refundPayment(UUID paymentId);

    @Transactional(readOnly = true)
    Object reconcile(String fromIso, String toIso);
}