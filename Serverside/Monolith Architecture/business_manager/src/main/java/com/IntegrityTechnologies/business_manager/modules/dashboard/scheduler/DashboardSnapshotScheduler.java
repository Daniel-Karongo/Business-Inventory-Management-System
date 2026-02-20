package com.IntegrityTechnologies.business_manager.modules.dashboard.scheduler;

import com.IntegrityTechnologies.business_manager.modules.dashboard.config.DashboardSnapshotProperties;
import com.IntegrityTechnologies.business_manager.modules.dashboard.service.DashboardSnapshotService;
import jakarta.annotation.PostConstruct;
import lombok.RequiredArgsConstructor;
import org.springframework.scheduling.TaskScheduler;
import org.springframework.stereotype.Component;

import java.time.*;
import java.util.Date;

@Component
@RequiredArgsConstructor
public class DashboardSnapshotScheduler {

    private final DashboardSnapshotService snapshotService;
    private final DashboardSnapshotProperties properties;
    private final TaskScheduler taskScheduler;

    @PostConstruct
    public void scheduleDailySnapshot() {

        String[] parts = properties.getTime().split(":");
        int hour = Integer.parseInt(parts[0]);
        int minute = Integer.parseInt(parts[1]);

        Runnable task = () -> {
            LocalDate yesterday = LocalDate.now().minusDays(1);
            snapshotService.compute(yesterday);
        };

        taskScheduler.scheduleAtFixedRate(
                task,
                nextExecution(hour, minute),
                24 * 60 * 60 * 1000L   // 24 hours in ms
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