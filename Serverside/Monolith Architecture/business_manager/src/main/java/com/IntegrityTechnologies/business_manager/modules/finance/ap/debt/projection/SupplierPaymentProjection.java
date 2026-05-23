package com.IntegrityTechnologies.business_manager.modules.finance.ap.debt.projection;

import com.IntegrityTechnologies.business_manager.modules.finance.ap.payment.enums.SupplierPaymentMethod;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.payment.enums.SupplierPaymentStatus;
import com.IntegrityTechnologies.business_manager.modules.finance.shared.enums.FinancialPostingStatus;

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

    SupplierPaymentStatus getStatus();

    SupplierPaymentMethod getPaymentMethod();

    String getReference();

    boolean getPosted();

    boolean getReversed();

    FinancialPostingStatus getPostingStatus();
}