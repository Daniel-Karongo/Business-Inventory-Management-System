package com.IntegrityTechnologies.business_manager.modules.platform.observability.controller;

import com.IntegrityTechnologies.business_manager.modules.platform.observability.service.TenantMetricsService;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/platform/metrics")
@RequiredArgsConstructor
public class MetricsController {

    private final TenantMetricsService metrics;

    @GetMapping("/requests")
    public long totalRequests() {

        return metrics.totalRequests();

    }

}