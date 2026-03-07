package com.IntegrityTechnologies.business_manager.modules.platform.observability.controller;

import com.IntegrityTechnologies.business_manager.modules.platform.observability.service.TenantMetricsService;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.PlatformAdminOnly;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

@Tag(name = "Platform Metrics")
@RestController
@RequestMapping("/api/platform/metrics")
@RequiredArgsConstructor
@PlatformAdminOnly
public class MetricsController {

    private final TenantMetricsService metrics;

    @GetMapping("/requests")
    public long totalRequests() {

        return metrics.totalRequests();

    }

}