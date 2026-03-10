package com.IntegrityTechnologies.business_manager.modules.platform.observability.controller;

import com.IntegrityTechnologies.business_manager.modules.platform.observability.entity.TenantUsageMetric;
import com.IntegrityTechnologies.business_manager.modules.platform.observability.repository.TenantUsageMetricRepository;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.PlatformAdminOnly;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/platform/analytics")
@RequiredArgsConstructor
@PlatformAdminOnly
public class TenantMetricsController {

    private final TenantUsageMetricRepository repository;

    @GetMapping
    public Page<TenantUsageMetric> metrics(Pageable pageable) {

        return repository.findAll(pageable);

    }

}