package com.IntegrityTechnologies.business_manager.modules.finance.accounting.scheduler;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.service.TenantExecutionService;
import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.repository.BranchRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.service.PeriodClosingService;
import lombok.RequiredArgsConstructor;
import org.springframework.boot.context.event.ApplicationReadyEvent;
import org.springframework.context.event.EventListener;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class PeriodClosingScheduler {

    private final PeriodClosingService closingService;
    private final BranchRepository branchRepository;
    private final TenantExecutionService tenantExecutionService;

    @Scheduled(cron = "${accounting.period.close.cron}")
    public void scheduledMonthlyClose() {

        tenantExecutionService.forEachTenant(tenantId -> {

            branchRepository.findByTenantIdAndDeletedFalse(tenantId)
                    .forEach(branch -> {

                        closingService.autoClosePreviousMonthIfNeeded(
                                "SYSTEM",
                                branch.getId()
                        );

                    });

        });

    }

    @EventListener(ApplicationReadyEvent.class)
    public void recoverMissedClosures() {

        tenantExecutionService.forEachTenant(tenantId -> {

            branchRepository.findByTenantIdAndDeletedFalse(tenantId)
                    .forEach(branch -> {

                        closingService.closeAllOverduePeriods(
                                "SYSTEM",
                                branch.getId()
                        );

                    });

        });

    }

}