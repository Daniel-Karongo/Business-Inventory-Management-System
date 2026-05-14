package com.IntegrityTechnologies.business_manager.modules.platform.observability.service;

import com.IntegrityTechnologies.business_manager.modules.platform.observability.repository.TenantUsageMetricRepository;
import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;

@Component
@RequiredArgsConstructor
public class MetricsRetentionJob {

    private final TenantUsageMetricRepository repository;

    @Scheduled(cron = "0 0 3 * * *")
    @Transactional
    public void cleanup() {

        repository.deleteBySnapshotTimeBefore(
                LocalDateTime.now().minusDays(30)
        );
    }
}