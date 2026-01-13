package com.IntegrityTechnologies.business_manager.modules.finance.accounting.dto;

import java.math.BigDecimal;
import java.util.UUID;

public record LedgerLineResponse(
        UUID accountId,
        String accountCode,
        String accountName,
        String direction,
        BigDecimal amount
) {}