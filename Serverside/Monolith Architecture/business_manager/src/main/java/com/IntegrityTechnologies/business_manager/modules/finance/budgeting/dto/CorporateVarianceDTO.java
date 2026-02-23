package com.IntegrityTechnologies.business_manager.modules.finance.budgeting.dto;

import lombok.Builder;
import lombok.Data;

import java.math.BigDecimal;
import java.util.UUID;

@Data
@Builder
public class CorporateVarianceDTO {

    private int fiscalYear;
    private int month;
    private UUID accountId;

    private BigDecimal planned;
    private BigDecimal actual;
    private BigDecimal variance;
}