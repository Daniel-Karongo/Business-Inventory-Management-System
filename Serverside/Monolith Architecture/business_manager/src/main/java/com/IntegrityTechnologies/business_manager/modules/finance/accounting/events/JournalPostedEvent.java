package com.IntegrityTechnologies.business_manager.modules.finance.accounting.events;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

public record JournalPostedEvent(
        UUID tenantId,
        UUID branchId,
        UUID journalId,
        UUID periodId,
        LocalDate accountingDate,
        LocalDateTime postedAt,

        String sourceModule,
        List<LedgerEntryDTO> entries
) {}