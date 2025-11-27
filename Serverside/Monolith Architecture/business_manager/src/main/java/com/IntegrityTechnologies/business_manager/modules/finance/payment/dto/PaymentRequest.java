package com.IntegrityTechnologies.business_manager.modules.finance.payment.dto;

import lombok.Data;

import java.math.BigDecimal;
import java.util.UUID;

@Data
public class PaymentRequest {
    private UUID saleId;          // the sale this payment is for
    private BigDecimal amount;
    private String method;        // CASH, MPESA, CARD etc.
    private String providerReference; // optional
    private String note;          // optional
}