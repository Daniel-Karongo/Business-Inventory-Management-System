package com.IntegrityTechnologies.business_manager.modules.finance.accounting.governance;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class GovernanceAuditService {

    private final GovernanceAuditLogRepository repository;

    public void log(
            UUID branchId,
            String action,
            String performedBy,
            String details
    ) {

        GovernanceAuditLog log = new GovernanceAuditLog();
        log.setBranchId(branchId);
        log.setAction(action);
        log.setPerformedBy(performedBy);
        log.setPerformedAt(LocalDateTime.now());
        log.setDetails(details);

        repository.save(log);
    }
}