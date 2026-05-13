package com.IntegrityTechnologies.business_manager.modules.finance.payment.base.service;

import com.IntegrityTechnologies.business_manager.modules.finance.payment.base.dto.PaymentRequest;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.base.dto.PaymentDTO;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.data.domain.Page;

import java.math.BigDecimal;
import java.util.UUID;

public interface PaymentService {

    PaymentDTO processPayment(
            UUID branchId,
            PaymentRequest request
    );

    PaymentDTO getPayment(
            UUID branchId,
            UUID paymentId
    );

    @Transactional(readOnly = true)
    Page<PaymentDTO> listPayments(
            UUID branchId,
            int page,
            int size,
            String method,
            String status,
            UUID saleId
    );

    @Transactional
    PaymentDTO refundPayment(
            UUID branchId,
            UUID paymentId
    );

    @Transactional(readOnly = true)
    Object reconcile(
            UUID branchId,
            String fromIso,
            String toIso
    );

    PaymentDTO reversePayment(
            UUID branchId,
            UUID id,
            String note
    );

    BigDecimal totalSuccessfulPaymentsForSale(
            UUID branchId,
            UUID saleId
    );

    void validatePaymentRequest(
            UUID branchId,
            PaymentRequest req
    );
}