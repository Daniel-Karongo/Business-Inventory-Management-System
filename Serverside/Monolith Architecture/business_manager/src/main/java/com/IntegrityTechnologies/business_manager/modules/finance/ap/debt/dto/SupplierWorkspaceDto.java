package com.IntegrityTechnologies.business_manager.modules.finance.ap.debt.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;
import java.util.List;
import java.util.UUID;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class SupplierWorkspaceDto {

    private UUID supplierId;

    private String supplierName;

    private BigDecimal totalOutstanding;
    private BigDecimal overdueAmount;
    private BigDecimal unappliedPayments;
    private BigDecimal netPayable;

    private long openBills;
    private long overdueBills;

    private List<SupplierBillDto> bills;

    private List<SupplierPaymentDto> payments;

    private List<SupplierTimelineEntryDto> timeline;
}