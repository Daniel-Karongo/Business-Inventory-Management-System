package com.IntegrityTechnologies.business_manager.modules.finance.payment.service;

import com.IntegrityTechnologies.business_manager.modules.finance.payment.dto.PaymentRequest;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.dto.PaymentDTO;
import org.springframework.data.domain.Page;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.util.UUID;

public interface PaymentService {
    PaymentDTO processPayment(PaymentRequest request);
    PaymentDTO getPayment(UUID paymentId);

    @Transactional(readOnly = true)
    Page<PaymentDTO> listPayments(int page, int size, String method, String status, UUID saleId);

    @Transactional
    PaymentDTO refundPayment(UUID paymentId);

    @Transactional(readOnly = true)
    Object reconcile(String fromIso, String toIso);

    PaymentDTO reversePayment(UUID id, String note);
    BigDecimal totalSuccessfulPaymentsForSale(UUID saleId);
    void validatePaymentRequest(PaymentRequest req);

}