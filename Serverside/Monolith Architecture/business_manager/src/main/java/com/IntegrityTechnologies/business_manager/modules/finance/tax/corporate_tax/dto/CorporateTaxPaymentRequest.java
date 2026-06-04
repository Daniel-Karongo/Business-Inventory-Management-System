package com.IntegrityTechnologies.business_manager.modules.finance.tax.corporate_tax.dto;

import lombok.Data;

import java.math.BigDecimal;
import java.util.UUID;

@Data
public class CorporateTaxPaymentRequest {

    private UUID accountId;

    private BigDecimal amount;
}