package com.IntegrityTechnologies.business_manager.modules.platform.security.rate;

import com.IntegrityTechnologies.business_manager.modules.platform.subscription.entity.Plan;
import com.IntegrityTechnologies.business_manager.modules.platform.subscription.service.SubscriptionService;
import com.IntegrityTechnologies.business_manager.security.cache.TenantMetadataCache;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.time.Instant;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;

@Component
@RequiredArgsConstructor
public class TenantRateLimiter {

    private final SubscriptionService subscriptionService;

    private static final long WINDOW_SECONDS = 60;

    private final Map<UUID, Counter> counters = new ConcurrentHashMap<>();
    private final TenantMetadataCache tenantMetadataCache;

    public boolean allow(UUID tenantId) {

        int limit = resolveLimit(tenantId);

        Counter counter = counters.computeIfAbsent(
                tenantId,
                t -> new Counter()
        );

        synchronized (counter) {

            long now = Instant.now().getEpochSecond();

            if (now - counter.windowStart >= WINDOW_SECONDS) {
                counter.windowStart = now;
                counter.count = 0;
            }

            counter.count++;

            return counter.count <= limit;
        }
    }

    public int resolveLimit(UUID tenantId) {

        return tenantMetadataCache
                .getSubscriptionPlan(tenantId)
                .getRequestsPerMinute();
    }

    private static class Counter {

        long windowStart = Instant.now().getEpochSecond();

        int count = 0;
    }
}