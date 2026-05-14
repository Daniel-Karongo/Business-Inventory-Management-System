package com.IntegrityTechnologies.business_manager.modules.platform.observability.service;

import lombok.RequiredArgsConstructor;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class MetricsCleanupScheduler {

    private final TenantMetricsService tenantMetricsService;

    @Scheduled(fixedDelay = 3600000)
    public void cleanup() {
        tenantMetricsService.evictStaleTenants();
    }
}