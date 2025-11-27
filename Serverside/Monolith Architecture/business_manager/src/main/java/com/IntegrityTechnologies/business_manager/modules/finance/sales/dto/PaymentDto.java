package com.IntegrityTechnologies.business_manager.modules.finance.sales.dto;

import lombok.Data;

import java.math.BigDecimal;

@Data
public class PaymentDto {
    private BigDecimal amount;
    private String method; // CASH, MPESA, CARD
    private String reference; // provider reference (e.g., mpesa checkout)
}
