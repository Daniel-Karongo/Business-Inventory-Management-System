package com.IntegrityTechnologies.business_manager.modules.finance.accounting.scheduler;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.service.PeriodClosingService;
import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.repository.BranchRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.context.event.EventListener;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;
import org.springframework.boot.context.event.ApplicationReadyEvent;

@Component
@RequiredArgsConstructor
public class PeriodClosingScheduler {

    private final PeriodClosingService closingService;
    private final BranchRepository branchRepository;

    @Scheduled(cron = "${accounting.period.close.cron}")
    public void scheduledMonthlyClose() {

        branchRepository.findAll().forEach(branch ->
                closingService.autoClosePreviousMonthIfNeeded(
                        "SYSTEM",
                        branch.getId()
                )
        );
    }

    @EventListener(ApplicationReadyEvent.class)
    public void recoverMissedClosures() {

        branchRepository.findAll().forEach(branch ->
                closingService.closeAllOverduePeriods(
                        "SYSTEM",
                        branch.getId()
                )
        );
    }
}