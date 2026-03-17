package com.IntegrityTechnologies.business_manager.config.kafka;

import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;
import java.util.UUID;

@Component
@RequiredArgsConstructor
public class OutboxCleanupService {

    private final EventOutboxRepository repo;

    @Scheduled(cron = "0 0 3 * * *")
    public void cleanup() {

        UUID tenantId = TenantContext.getTenantId();

        if (tenantId == null) {
            return;
        }

        LocalDateTime cutoff =
                LocalDateTime.now().minusDays(7);

        repo.deleteProcessedBefore(
                tenantId,
                cutoff
        );
    }
}