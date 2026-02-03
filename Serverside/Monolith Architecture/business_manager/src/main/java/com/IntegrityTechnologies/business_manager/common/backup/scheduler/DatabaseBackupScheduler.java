package com.IntegrityTechnologies.business_manager.common.backup.scheduler;

import com.IntegrityTechnologies.business_manager.common.backup.config.DatabaseBackupProperties;
import com.IntegrityTechnologies.business_manager.common.backup.service.DatabaseBackupService;
import jakarta.annotation.PostConstruct;
import lombok.RequiredArgsConstructor;
import org.springframework.scheduling.TaskScheduler;
import org.springframework.scheduling.concurrent.ConcurrentTaskScheduler;
import org.springframework.scheduling.support.CronTrigger;
import org.springframework.stereotype.Component;

import java.time.*;
import java.time.temporal.TemporalAdjusters;

@Component
@RequiredArgsConstructor
public class DatabaseBackupScheduler {

    private final DatabaseBackupService service;
    private final DatabaseBackupProperties props;
    private final TaskScheduler scheduler = new ConcurrentTaskScheduler();

    @PostConstruct
    public void schedule() {

        if (!props.isEnabled()) return;

        // Custom cron
        if ("custom".equalsIgnoreCase(props.getMode())) {
            scheduler.schedule(service::backupNow, new CronTrigger(props.getCron()));
            return;
        }

        // Daily / Weekly / Monthly
        scheduler.schedule(service::backupNow, triggerContext -> {

            LocalTime time = LocalTime.parse(props.getTime());
            LocalDateTime next = LocalDateTime.now().with(time);

            switch (props.getMode().toLowerCase()) {
                case "weekly" ->
                        next = next.with(TemporalAdjusters.nextOrSame(
                                DayOfWeek.valueOf(props.getWeekday())));

                case "monthly" -> {
                    next = next.withDayOfMonth(props.getMonthday());
                    if (next.isBefore(LocalDateTime.now())) {
                        next = next.plusMonths(1);
                    }
                }

                default -> {
                    if (next.isBefore(LocalDateTime.now())) {
                        next = next.plusDays(1);
                    }
                }
            }

            // ✅ Spring 6 expects Instant, not Date
            return next.atZone(ZoneId.systemDefault()).toInstant();
        });
    }
}