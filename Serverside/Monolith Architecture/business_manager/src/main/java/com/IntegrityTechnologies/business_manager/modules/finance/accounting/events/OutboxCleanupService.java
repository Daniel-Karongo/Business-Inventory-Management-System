package com.IntegrityTechnologies.business_manager.modules.finance.accounting.events;

import lombok.RequiredArgsConstructor;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;

@Component
@RequiredArgsConstructor
public class OutboxCleanupService {

    private final EventOutboxRepository repo;

    @Scheduled(cron = "0 0 3 * * *")
    public void cleanup() {

        LocalDateTime cutoff =
                LocalDateTime.now().minusDays(7);

        repo.deleteProcessedBefore(cutoff);
    }
}