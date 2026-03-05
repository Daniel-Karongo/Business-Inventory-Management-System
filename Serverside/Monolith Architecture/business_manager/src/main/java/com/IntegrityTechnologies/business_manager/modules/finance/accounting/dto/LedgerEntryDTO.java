package com.IntegrityTechnologies.business_manager.modules.finance.accounting.dto;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.EntryDirection;

import java.math.BigDecimal;
import java.util.UUID;

public record LedgerEntryDTO(
        UUID accountId,
        EntryDirection direction,
        BigDecimal amount
) {}