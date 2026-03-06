package com.IntegrityTechnologies.business_manager.modules.finance.accounting.dto;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.governance.GovernanceAuditLog;

import java.time.LocalDateTime;
import java.util.UUID;

public record GovernanceAuditLogResponse(

        UUID id,
        UUID branchId,
        String action,
        String performedBy,
        String details,
        LocalDateTime performedAt
) {

    public static GovernanceAuditLogResponse from(GovernanceAuditLog log) {

        return new GovernanceAuditLogResponse(
                log.getId(),
                log.getBranchId(),
                log.getAction(),
                log.getPerformedBy(),
                log.getDetails(),
                log.getPerformedAt()
        );
    }
}