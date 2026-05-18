package com.IntegrityTechnologies.business_manager.modules.finance.ap.reporting.dto;

import lombok.Builder;
import lombok.Data;

import java.math.BigDecimal;
import java.util.UUID;

@Data
@Builder
public class AccountsPayableSummaryRowDto {

    private UUID supplierId;

    private String supplierName;

    private long openInvoiceCount;

    private BigDecimal totalOutstanding;

    private BigDecimal unappliedPayments;

    private BigDecimal netExposure;

    private long overdueInvoiceCount;
}