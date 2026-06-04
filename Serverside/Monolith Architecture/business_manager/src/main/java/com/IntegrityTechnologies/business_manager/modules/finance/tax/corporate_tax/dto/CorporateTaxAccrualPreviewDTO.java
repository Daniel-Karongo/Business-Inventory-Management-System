package com.IntegrityTechnologies.business_manager.modules.finance.tax.corporate_tax.dto;

import lombok.Builder;
import lombok.Data;

import java.math.BigDecimal;
import java.time.LocalDate;

@Data
@Builder
public class CorporateTaxAccrualPreviewDTO {

    private java.util.UUID periodId;

    private LocalDate startDate;

    private LocalDate endDate;

    private BigDecimal revenue;

    private BigDecimal expenses;

    private BigDecimal taxableProfit;

    private BigDecimal taxRate;

    private BigDecimal estimatedTax;
}