package com.IntegrityTechnologies.business_manager.modules.finance.accounting.dto;

import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

public record JournalResponse(
        UUID id,
        String reference,
        String description,
        String sourceModule,
        String postedBy,
        LocalDateTime postedAt,
        List<LedgerLineResponse> entries
) {}