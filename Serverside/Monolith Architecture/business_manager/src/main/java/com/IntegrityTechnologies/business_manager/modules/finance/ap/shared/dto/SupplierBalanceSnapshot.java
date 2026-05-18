package com.IntegrityTechnologies.business_manager.modules.finance.ap.shared.dto;

import lombok.Builder;
import lombok.Data;

import java.math.BigDecimal;
import java.util.UUID;

@Data
@Builder
public class SupplierBalanceSnapshot {

    private UUID supplierId;

    private BigDecimal totalOutstandingInvoices;

    private BigDecimal totalUnappliedPayments;

    private BigDecimal totalCredits;

    private BigDecimal netPayablePosition;

    private long openInvoiceCount;

    private long overdueInvoiceCount;
}