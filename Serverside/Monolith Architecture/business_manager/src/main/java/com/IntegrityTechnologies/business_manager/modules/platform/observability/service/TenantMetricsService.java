package com.IntegrityTechnologies.business_manager.modules.platform.observability.service;

import org.springframework.stereotype.Service;

import java.util.concurrent.atomic.AtomicLong;

@Service
public class TenantMetricsService {

    private final AtomicLong requests = new AtomicLong();

    public void recordRequest() {
        requests.incrementAndGet();
    }

    public long totalRequests() {
        return requests.get();
    }

}