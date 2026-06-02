package com.IntegrityTechnologies.business_manager.modules.finance.accounting.scheduler;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.seed.AccountingPeriodBootstrapService;
import com.IntegrityTechnologies.business_manager.modules.person.branch.repository.BranchRepository;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.service.TenantExecutionService;
import lombok.RequiredArgsConstructor;
import org.springframework.boot.context.event.ApplicationReadyEvent;
import org.springframework.context.event.EventListener;
import org.springframework.core.annotation.Order;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class AccountingPeriodStartupReconciler {

    private final TenantExecutionService tenantExecutionService;
    private final BranchRepository branchRepository;
    private final AccountingPeriodBootstrapService bootstrapService;

    @EventListener(ApplicationReadyEvent.class)
    @Order(1)
    public void reconcile() {

        tenantExecutionService.forEachTenant(tenantId ->

                branchRepository
                        .findByTenantIdAndDeletedFalse(tenantId)
                        .forEach(branch ->

                                bootstrapService.ensurePeriods(
                                        tenantId,
                                        branch.getId()
                                )
                        )
        );
    }
}