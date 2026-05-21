package com.IntegrityTechnologies.business_manager.modules.finance.ap.debt.projection;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.UUID;

public interface SupplierPaymentProjection {

    UUID getPaymentId();

    String getPaymentNumber();

    LocalDate getPaymentDate();

    BigDecimal getAmount();

    BigDecimal getAllocatedAmount();

    BigDecimal getUnappliedAmount();

    String getPaymentMethod();

    String getReference();
}