package com.IntegrityTechnologies.business_manager.modules.finance.accounting.events;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.dto.LedgerEntryDTO;

import java.util.List;
import java.util.UUID;

public record JournalPostedEvent(
        UUID journalId,
        UUID branchId,
        List<LedgerEntryDTO> entries
) {}