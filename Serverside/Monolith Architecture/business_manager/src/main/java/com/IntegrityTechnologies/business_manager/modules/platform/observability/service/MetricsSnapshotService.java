package com.IntegrityTechnologies.business_manager.modules.platform.observability.service;

import com.IntegrityTechnologies.business_manager.modules.platform.observability.entity.TenantUsageMetric;
import com.IntegrityTechnologies.business_manager.modules.platform.observability.repository.TenantUsageMetricRepository;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.service.TenantExecutionService;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.Map;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class MetricsSnapshotService {

    private final TenantMetricsService metricsService;
    private final TenantUsageMetricRepository repository;
    private final TenantExecutionService tenantExecutionService;

    @Scheduled(fixedRate = 300000)
    public void snapshotMetrics() {

        Map<UUID, Long> requests =
                metricsService.snapshotTenantRequests();

        Map<UUID, Long> errors =
                metricsService.snapshotTenantErrors();

        tenantExecutionService.forEachTenant(tenantId -> {

            long reqCount = requests.getOrDefault(tenantId, 0L);
            long errCount = errors.getOrDefault(tenantId, 0L);

            TenantUsageMetric metric =
                    TenantUsageMetric.builder()
                            .tenantId(tenantId)
                            .requests(reqCount)
                            .errors(errCount)
                            .snapshotTime(LocalDateTime.now())
                            .build();

            repository.save(metric);
        });
    }
}