package com.IntegrityTechnologies.business_manager.config.kafka;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.service.TenantExecutionService;
import lombok.RequiredArgsConstructor;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;

@Component
@RequiredArgsConstructor
public class OutboxCleanupService {

    private final EventOutboxRepository repo;
    private final TenantExecutionService tenantExecutionService;

    @Scheduled(cron = "0 0 3 * * *")
    public void cleanup() {

        LocalDateTime cutoff =
                LocalDateTime.now().minusDays(7);

        tenantExecutionService.forEachTenant(tenantId -> {

            repo.deleteProcessedBefore(
                    tenantId,
                    cutoff
            );

        });
    }
}