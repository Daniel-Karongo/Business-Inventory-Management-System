package com.IntegrityTechnologies.business_manager.modules.finance.accounting.projection;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.UUID;

public interface LedgerRunningBalanceProjection {

    UUID getJournalId();
    String getReference();
    LocalDateTime getPostedAt();
    String getDirection();
    BigDecimal getAmount();
    BigDecimal getRunningBalance();
}