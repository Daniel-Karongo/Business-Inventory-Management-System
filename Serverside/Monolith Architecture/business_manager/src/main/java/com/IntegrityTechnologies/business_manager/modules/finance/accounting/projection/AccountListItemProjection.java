package com.IntegrityTechnologies.business_manager.modules.finance.accounting.projection;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.AccountType;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.UUID;

public interface AccountListItemProjection {

    UUID getId();

    String getCode();

    String getName();

    AccountType getType();

    String getRole();

    boolean isActive();

    BigDecimal getBalance();

    LocalDateTime getUpdatedAt();
}