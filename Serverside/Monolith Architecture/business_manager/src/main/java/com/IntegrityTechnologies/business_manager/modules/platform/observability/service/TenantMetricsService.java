package com.IntegrityTechnologies.business_manager.modules.platform.observability.service;

import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import org.springframework.stereotype.Service;

import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicLong;

@Service
public class TenantMetricsService {

    /* =====================================================
       GLOBAL METRICS
    ===================================================== */

    private final AtomicLong totalRequests = new AtomicLong();
    private final AtomicLong totalErrors = new AtomicLong();

    /* =====================================================
       TENANT METRICS
    ===================================================== */

    private final Map<UUID, AtomicLong> tenantRequests =
            new ConcurrentHashMap<>();

    private final Map<UUID, AtomicLong> tenantErrors =
            new ConcurrentHashMap<>();


    /* =====================================================
       RECORD REQUEST
    ===================================================== */

    public void recordRequest() {

        totalRequests.incrementAndGet();

        UUID tenantId = TenantContext.getOrNull();

        if (tenantId == null) {
            return;
        }

        tenantRequests
                .computeIfAbsent(tenantId, t -> new AtomicLong())
                .incrementAndGet();
    }

    /* =====================================================
       RECORD ERROR
    ===================================================== */

    public void recordError() {

        totalErrors.incrementAndGet();

        UUID tenantId = TenantContext.getOrNull();

        if (tenantId == null) {
            return;
        }

        tenantErrors
                .computeIfAbsent(tenantId, t -> new AtomicLong())
                .incrementAndGet();
    }

    /* =====================================================
       GLOBAL STATS
    ===================================================== */

    public long totalRequests() {
        return totalRequests.get();
    }

    public long totalErrors() {
        return totalErrors.get();
    }

    /* =====================================================
       TENANT STATS
    ===================================================== */

    public long tenantRequests(UUID tenantId) {

        AtomicLong counter = tenantRequests.get(tenantId);

        return counter == null ? 0 : counter.get();
    }

    public long tenantErrors(UUID tenantId) {

        AtomicLong counter = tenantErrors.get(tenantId);

        return counter == null ? 0 : counter.get();
    }

    public Map<UUID, Long> snapshotTenantRequests() {

        Map<UUID, Long> snapshot = new ConcurrentHashMap<>();

        tenantRequests.forEach((tenantId, counter) ->
                snapshot.put(tenantId, counter.get()));

        return snapshot;
    }

    public Map<UUID, Long> snapshotTenantErrors() {

        Map<UUID, Long> snapshot = new ConcurrentHashMap<>();

        tenantErrors.forEach((tenantId, counter) ->
                snapshot.put(tenantId, counter.get()));

        return snapshot;
    }
}