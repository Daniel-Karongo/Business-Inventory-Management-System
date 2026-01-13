package com.IntegrityTechnologies.business_manager.modules.finance.accounting.dto;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.EntryDirection;

import java.math.BigDecimal;
import java.util.List;
import java.util.UUID;

public record ManualJournalRequest(
        String reference,
        String description,
        List<Line> lines
) {
    public record Line(
            UUID accountId,
            EntryDirection direction,
            BigDecimal amount
    ) {}
}