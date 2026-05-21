package com.IntegrityTechnologies.business_manager.modules.finance.ap.debt.projection;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.UUID;

public interface SupplierDebtSummaryProjection {
    UUID getSupplierId();
    String getSupplierName();

    BigDecimal getTotalOutstanding();
    BigDecimal getOverdueAmount();
    BigDecimal getUnappliedPayments();
    BigDecimal getNetPayable();

    long getOpenBills();
    long getOverdueBills();

    LocalDate getOldestDueDate();
    LocalDate getLastPaymentDate();
}