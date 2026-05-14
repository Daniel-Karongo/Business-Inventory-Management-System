package com.IntegrityTechnologies.business_manager.modules.platform.observability.service;

import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import org.springframework.stereotype.Service;

import java.time.Instant;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicLong;

@Service
public class TenantMetricsService {

    private static final long STALE_TENANT_EVICTION_MS =
            24L * 60L * 60L * 1000L;

    /*
     =====================================================
     GLOBAL METRICS
     =====================================================
     */

    private final AtomicLong totalRequests =
            new AtomicLong();

    private final AtomicLong totalErrors =
            new AtomicLong();

    /*
     =====================================================
     INTERVAL METRICS
     =====================================================
     */

    private final Map<UUID, MetricCounter> tenantRequests =
            new ConcurrentHashMap<>();

    private final Map<UUID, MetricCounter> tenantErrors =
            new ConcurrentHashMap<>();

    /*
     =====================================================
     RECORD REQUEST
     =====================================================
     */

    public void recordRequest() {

        totalRequests.incrementAndGet();

        UUID tenantId =
                TenantContext.getOrNull();

        if (tenantId == null) {
            return;
        }

        tenantRequests
                .computeIfAbsent(
                        tenantId,
                        t -> new MetricCounter()
                )
                .increment();
    }

    /*
     =====================================================
     RECORD ERROR
     =====================================================
     */

    public void recordError() {

        totalErrors.incrementAndGet();

        UUID tenantId =
                TenantContext.getOrNull();

        if (tenantId == null) {
            return;
        }

        tenantErrors
                .computeIfAbsent(
                        tenantId,
                        t -> new MetricCounter()
                )
                .increment();
    }

    /*
     =====================================================
     GLOBAL STATS
     =====================================================
     */

    public long totalRequests() {
        return totalRequests.get();
    }

    public long totalErrors() {
        return totalErrors.get();
    }

    /*
     =====================================================
     SNAPSHOT + RESET
     =====================================================
     */

    public Map<UUID, Long> snapshotAndResetTenantRequests() {

        Map<UUID, Long> snapshot =
                new HashMap<>();

        tenantRequests.forEach((tenantId, counter) ->
                snapshot.put(
                        tenantId,
                        counter.getAndReset()
                ));

        return Map.copyOf(snapshot);
    }

    public Map<UUID, Long> snapshotAndResetTenantErrors() {

        Map<UUID, Long> snapshot =
                new HashMap<>();

        tenantErrors.forEach((tenantId, counter) ->
                snapshot.put(
                        tenantId,
                        counter.getAndReset()
                ));

        return Map.copyOf(snapshot);
    }

    /*
     =====================================================
     CLEANUP
     =====================================================
     */

    public void evictStaleTenants() {

        long cutoff =
                Instant.now().toEpochMilli()
                        - STALE_TENANT_EVICTION_MS;

        tenantRequests.entrySet()
                .removeIf(e ->
                        e.getValue().lastUpdated < cutoff
                );

        tenantErrors.entrySet()
                .removeIf(e ->
                        e.getValue().lastUpdated < cutoff
                );
    }

    /*
     =====================================================
     INTERNAL COUNTER
     =====================================================
     */

    private static class MetricCounter {

        private final AtomicLong counter =
                new AtomicLong();

        private volatile long lastUpdated =
                Instant.now().toEpochMilli();

        void increment() {

            counter.incrementAndGet();

            lastUpdated =
                    Instant.now().toEpochMilli();
        }

        long getAndReset() {

            lastUpdated =
                    Instant.now().toEpochMilli();

            return counter.getAndSet(0);
        }
    }
}