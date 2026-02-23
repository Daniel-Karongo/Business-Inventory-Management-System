package com.IntegrityTechnologies.business_manager.modules.finance.budgeting.scheduler;

import com.IntegrityTechnologies.business_manager.modules.finance.budgeting.config.BudgetSnapshotProperties;
import com.IntegrityTechnologies.business_manager.modules.finance.budgeting.service.BudgetService;
import jakarta.annotation.PostConstruct;
import lombok.RequiredArgsConstructor;
import org.springframework.scheduling.TaskScheduler;
import org.springframework.stereotype.Component;

import java.time.*;
import java.util.Date;

@Component
@RequiredArgsConstructor
public class BudgetSnapshotScheduler {

    private final BudgetService budgetService;
    private final BudgetSnapshotProperties properties;
    private final TaskScheduler taskScheduler;

    @PostConstruct
    public void scheduleDailySnapshot() {

        String[] parts = properties.getTime().split(":");
        int hour = Integer.parseInt(parts[0]);
        int minute = Integer.parseInt(parts[1]);

        Runnable task = () -> {
            LocalDate now = LocalDate.now();
            int fiscalYear = now.getYear();
            int month = now.getMonthValue();

            // Global snapshot
            budgetService.computeMonthlySnapshot(null, fiscalYear, month);
        };

        taskScheduler.scheduleAtFixedRate(
                task,
                nextExecution(hour, minute),
                24 * 60 * 60 * 1000L
        );
    }

    private Date nextExecution(int hour, int minute) {

        LocalDateTime now = LocalDateTime.now();
        LocalDateTime next = now.withHour(hour).withMinute(minute).withSecond(0);

        if (now.isAfter(next)) {
            next = next.plusDays(1);
        }

        return Date.from(next.atZone(ZoneId.systemDefault()).toInstant());
    }
}