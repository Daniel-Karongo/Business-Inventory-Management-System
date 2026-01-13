package com.IntegrityTechnologies.business_manager.modules.finance.accounting.dto;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.UUID;

public record LedgerRowResponse(
        UUID journalId,
        String journalReference,
        LocalDateTime postedAt,
        String direction,
        BigDecimal amount,
        BigDecimal runningBalance
) {}