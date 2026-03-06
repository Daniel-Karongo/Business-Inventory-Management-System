package com.IntegrityTechnologies.business_manager.modules.finance.accounting.cache;

import java.util.UUID;

public record AccountingLedgerUpdatedEvent(
        UUID branchId
) {}