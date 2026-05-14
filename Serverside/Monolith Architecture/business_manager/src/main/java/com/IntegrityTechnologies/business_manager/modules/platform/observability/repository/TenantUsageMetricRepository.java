package com.IntegrityTechnologies.business_manager.modules.platform.observability.repository;

import com.IntegrityTechnologies.business_manager.modules.platform.observability.entity.TenantUsageMetric;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.UUID;

public interface TenantUsageMetricRepository
        extends JpaRepository<TenantUsageMetric, UUID> {
    @Transactional
    void deleteBySnapshotTimeBefore(
            LocalDateTime cutoff
    );
}