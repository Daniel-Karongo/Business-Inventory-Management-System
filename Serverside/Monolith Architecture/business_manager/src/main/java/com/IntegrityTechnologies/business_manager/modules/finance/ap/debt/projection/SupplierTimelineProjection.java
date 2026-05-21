package com.IntegrityTechnologies.business_manager.modules.finance.ap.debt.projection;

import java.math.BigDecimal;
import java.time.LocalDate;

public interface SupplierTimelineProjection {

    LocalDate getTimestamp();

    String getActivity();

    String getReference();

    BigDecimal getDebitAmount();

    BigDecimal getCreditAmount();
}