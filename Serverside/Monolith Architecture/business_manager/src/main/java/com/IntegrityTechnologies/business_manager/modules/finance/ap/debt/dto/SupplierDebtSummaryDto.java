package com.IntegrityTechnologies.business_manager.modules.finance.ap.debt.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.UUID;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class SupplierDebtSummaryDto {
    private UUID supplierId;
    private String supplierName;

    private boolean hasOverdue;
    private boolean hasUnappliedPayments;

    private BigDecimal totalOutstanding;
    private BigDecimal overdueAmount;
    private BigDecimal unappliedPayments;
    private BigDecimal netPayable;

    private long openBills;
    private long overdueBills;

    private LocalDate oldestDueDate;
    private LocalDate lastPaymentDate;

    private String riskLevel;
}