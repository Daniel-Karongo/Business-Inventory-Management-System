package com.IntegrityTechnologies.business_manager.modules.finance.tax.corporate_tax.dto;

import lombok.Builder;
import lombok.Data;

import java.math.BigDecimal;

@Data
@Builder
public class CorporateTaxOverviewDTO {

    private BigDecimal outstandingTax;

    private BigDecimal estimatedTax;

    private BigDecimal taxableProfit;

    private long pendingAccruals;

    private long pendingPayments;

    private BigDecimal totalPaidTax;

    private BigDecimal totalAccruedTax;
}