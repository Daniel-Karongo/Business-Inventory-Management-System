package com.IntegrityTechnologies.business_manager.modules.platform.observability.service;

import com.IntegrityTechnologies.business_manager.modules.platform.observability.entity.TenantUsageMetric;
import com.IntegrityTechnologies.business_manager.modules.platform.observability.repository.TenantUsageMetricRepository;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.service.TenantExecutionService;
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

    @Scheduled(fixedDelay = 300000)
    public void snapshotMetrics() {

        Map<UUID, Long> requests =
                metricsService.snapshotAndResetTenantRequests();

        Map<UUID, Long> errors =
                metricsService.snapshotAndResetTenantErrors();

        LocalDateTime snapshotTime =
                LocalDateTime.now();

        tenantExecutionService.forEachTenant(tenantId -> {

            long reqCount =
                    requests.getOrDefault(tenantId, 0L);

            long errCount =
                    errors.getOrDefault(tenantId, 0L);

            TenantUsageMetric metric =
                    TenantUsageMetric.builder()
                            .tenantId(tenantId)
                            .requests(reqCount)
                            .errors(errCount)
                            .snapshotTime(snapshotTime)
                            .build();

            repository.save(metric);
        });
    }
}