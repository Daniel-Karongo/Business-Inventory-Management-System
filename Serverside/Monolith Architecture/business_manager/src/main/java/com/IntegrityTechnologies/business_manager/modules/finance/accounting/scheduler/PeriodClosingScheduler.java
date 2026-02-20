package com.IntegrityTechnologies.business_manager.modules.finance.accounting.scheduler;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.service.PeriodClosingService;
import lombok.RequiredArgsConstructor;
import org.springframework.context.event.EventListener;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;
import org.springframework.boot.context.event.ApplicationReadyEvent;

@Component
@RequiredArgsConstructor
public class PeriodClosingScheduler {

    private final PeriodClosingService closingService;

    /* ============================================================
       MONTHLY AUTO CLOSE (1st day 00:10)
    ============================================================ */
    @Scheduled(cron = "${accounting.period.close.cron}")
    public void scheduledMonthlyClose() {
        closingService.autoClosePreviousMonthIfNeeded("SYSTEM");
    }

    /* ============================================================
       STARTUP RECOVERY
    ============================================================ */
    @EventListener(ApplicationReadyEvent.class)
    public void recoverMissedClosures() {
        closingService.closeAllOverduePeriods("SYSTEM");
    }
}