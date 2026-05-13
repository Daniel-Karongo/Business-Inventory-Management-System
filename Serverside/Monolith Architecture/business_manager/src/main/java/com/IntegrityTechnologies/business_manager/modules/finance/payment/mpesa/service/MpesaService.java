package com.IntegrityTechnologies.business_manager.modules.finance.payment.mpesa.service;

import com.IntegrityTechnologies.business_manager.modules.finance.payment.mpesa.model.MpesaTransaction;

import java.math.BigDecimal;
import java.util.Map;
import java.util.UUID;

public interface MpesaService {

    MpesaTransaction initiateStk(
            UUID branchId,
            UUID saleId,
            String phone,
            BigDecimal amount
    );

    void handleStkCallback(
            UUID branchId,
            Map<String, Object> callback
    );

    default void handleC2BConfirm(
            UUID branchId,
            Map<String, Object> payload
    ) {
    }

    default void handleC2BValidate(
            UUID branchId,
            Map<String, Object> payload
    ) {
    }
}