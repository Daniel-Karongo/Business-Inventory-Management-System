package com.IntegrityTechnologies.business_manager.modules.finance.accounting.events;

import java.time.LocalDateTime;
import java.util.UUID;

public record AccountingPeriodClosedEvent(

        UUID periodId,
        UUID branchId,
        LocalDateTime start,
        LocalDateTime end

) {}