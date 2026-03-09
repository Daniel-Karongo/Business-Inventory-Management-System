package com.IntegrityTechnologies.business_manager.modules.platform.observability.repository;

import com.IntegrityTechnologies.business_manager.modules.platform.observability.entity.TenantUsageMetric;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.UUID;

public interface TenantUsageMetricRepository
        extends JpaRepository<TenantUsageMetric, UUID> {
}