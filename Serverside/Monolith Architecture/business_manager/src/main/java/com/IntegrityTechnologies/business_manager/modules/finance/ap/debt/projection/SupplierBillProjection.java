package com.IntegrityTechnologies.business_manager.modules.finance.ap.debt.projection;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.UUID;

public interface SupplierBillProjection {

    UUID getInvoiceId();

    String getBillNumber();

    LocalDate getInvoiceDate();

    LocalDate getDueDate();

    BigDecimal getTotalAmount();

    BigDecimal getPaidAmount();

    BigDecimal getRemainingAmount();

    String getStatus();
}