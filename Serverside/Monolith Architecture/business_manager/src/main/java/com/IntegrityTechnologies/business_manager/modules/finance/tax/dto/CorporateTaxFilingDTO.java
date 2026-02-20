package com.IntegrityTechnologies.business_manager.modules.finance.tax.dto;

import lombok.Builder;
import lombok.Data;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.UUID;

@Data
@Builder
public class CorporateTaxFilingDTO {

    private UUID id;
    private UUID periodId;
    private BigDecimal taxableProfit;
    private BigDecimal taxRate;
    private BigDecimal taxAmount;
    private boolean paid;
    private LocalDateTime filedAt;
    private LocalDateTime paidAt;
}