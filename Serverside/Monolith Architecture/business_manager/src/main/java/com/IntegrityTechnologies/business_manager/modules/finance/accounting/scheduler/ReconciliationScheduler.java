package com.IntegrityTechnologies.business_manager.modules.finance.accounting.scheduler;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.service.BalanceReconciliationService;
import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.repository.BranchRepository;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.service.TenantExecutionService;
import lombok.RequiredArgsConstructor;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class ReconciliationScheduler {

    private final TenantExecutionService tenantExecutionService;
    private final BalanceReconciliationService reconciliationService;
    private final BranchRepository branchRepository;

    @Scheduled(cron = "${accounting.reconciliation.cron}")
    public void runReconciliation() {

        tenantExecutionService.forEachTenant(tenantId -> {

            branchRepository.findByTenantIdAndDeletedFalse(tenantId)
                    .forEach(branch -> {

                        reconciliationService.runAndPersist(
                                branch.getId(),
                                false,
                                "SYSTEM"
                        );

                    });

        });
    }
}