package com.IntegrityTechnologies.business_manager.modules.finance.accounting.scheduler;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.security.JournalIntegrityService;
import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.repository.BranchRepository;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.service.TenantExecutionService;
import lombok.RequiredArgsConstructor;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class JournalIntegrityScheduler {

    private final TenantExecutionService tenantExecutionService;
    private final JournalIntegrityService integrityService;
    private final BranchRepository branchRepository;

    @Scheduled(cron = "${accounting.integrity.cron}")
    public void runIntegrityCheck() {

        tenantExecutionService.forEachTenant(tenantId -> {

            branchRepository.findByTenantIdAndDeletedFalse(tenantId)
                    .forEach(branch -> {

                        integrityService.performAndPersistAudit(
                                branch.getId()
                        );

                    });

        });
    }
}