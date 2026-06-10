package com.IntegrityTechnologies.business_manager.modules.finance.accounting.events;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.AccountType;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.EntryDirection;

import java.math.BigDecimal;
import java.util.UUID;

public record LedgerEntryDTO(
        UUID accountId,
        AccountType accountType,
        String accountRole,
        EntryDirection direction,
        BigDecimal amount
) {}