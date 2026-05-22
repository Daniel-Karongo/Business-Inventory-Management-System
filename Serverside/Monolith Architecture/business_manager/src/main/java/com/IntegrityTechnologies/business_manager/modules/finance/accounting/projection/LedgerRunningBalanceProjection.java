package com.IntegrityTechnologies.business_manager.modules.finance.accounting.projection;

import java.math.BigDecimal;
import java.time.LocalDateTime;

public interface LedgerRunningBalanceProjection {

    String getJournalId();

    String getReference();

    LocalDateTime getPostedAt();

    String getDirection();

    BigDecimal getAmount();

    BigDecimal getRunningBalance();
}