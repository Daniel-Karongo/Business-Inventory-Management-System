package com.IntegrityTechnologies.business_manager.modules.finance.payment.mpesa.service;

import com.IntegrityTechnologies.business_manager.modules.finance.payment.mpesa.model.MpesaTransaction;

import java.math.BigDecimal;
import java.util.Map;
import java.util.UUID;

public interface MpesaService {
    MpesaTransaction initiateStk(UUID saleId, String phone, BigDecimal amount);
    void handleStkCallback(Map<String, Object> callback);
}