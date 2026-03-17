package com.IntegrityTechnologies.business_manager.modules.finance.accounting.governance;

import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import com.IntegrityTechnologies.business_manager.security.util.BranchTenantGuard;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class GovernanceAuditService {

    private final GovernanceAuditLogRepository repository;
    private final BranchTenantGuard branchTenantGuard;

    public void log(
            UUID branchId,
            String action,
            String performedBy,
            String details
    ) {

        branchTenantGuard.validate(branchId);

        GovernanceAuditLog log = new GovernanceAuditLog();

        log.setTenantId(TenantContext.getTenantId());
        log.setBranchId(branchId);

        log.setAction(action);
        log.setPerformedBy(performedBy);
        log.setPerformedAt(LocalDateTime.now());
        log.setDetails(details);

        repository.save(log);
    }
}