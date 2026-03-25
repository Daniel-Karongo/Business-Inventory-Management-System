package com.IntegrityTechnologies.business_manager.config.caffeine;

import com.IntegrityTechnologies.business_manager.modules.finance.sales.sellable.cache.SellableCacheKey;
import com.IntegrityTechnologies.business_manager.security.util.BranchContext;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.cache.Cache;
import org.springframework.cache.CacheManager;
import org.springframework.cache.concurrent.ConcurrentMapCache;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Map;
import java.util.UUID;

@Component
@RequiredArgsConstructor
public class CacheInvalidationService {

    private final CacheManager cacheManager;

    private UUID tenantId() {
        return TenantContext.getTenantId();
    }

    private UUID branchId() {
        return BranchContext.getOrNull();
    }
    public void evictPackaging(UUID variantId) {
        Cache cache = cacheManager.getCache("packaging");
        if (cache != null) {
            cache.evict(
                    SellableCacheKey.packaging(variantId)
            );
        }
    }

    public void evictPricing(
            UUID variantId,
            UUID packagingId,
            UUID branchId,
            UUID customerId,
            UUID groupId,
            Long quantity
    ) {
        Cache cache = cacheManager.getCache("pricing-preview");

        if (cache != null) {
            cache.evict(
                    SellableCacheKey.pricing(
                            variantId,
                            packagingId,
                            branchId,
                            customerId,
                            groupId,
                            quantity
                    )
            );
        }
    }

    public void evictPricingByVariant(UUID variantId, UUID branchId) {
        Cache cache = cacheManager.getCache("pricing-preview");

        if (cache != null && cache instanceof org.springframework.cache.concurrent.ConcurrentMapCache cmc) {

            Map<Object, Object> nativeCache = cmc.getNativeCache();
            String prefix = TenantContext.getTenantId()
                    + "::pricing::" + variantId + "::" + branchId;

            nativeCache.keySet().removeIf(key ->
                    key.toString().startsWith(prefix)
            );
        }
    }

    public void evictVariantSearch(UUID branchId) {
        Cache cache = cacheManager.getCache("variant-search");

        if (cache != null && cache instanceof ConcurrentMapCache cmc) {
            Map<Object, Object> nativeCache = cmc.getNativeCache();

            nativeCache.keySet().removeIf(key ->
                    key.toString().contains("::" + branchId + "::")
            );
        }
    }

    public void evictCategoryCaches(UUID branch) {
        UUID tenant = tenantId();

        List<String> prefixes = List.of(
                tenant + "::" + branch + "::category::tree",
                tenant + "::" + branch + "::category::flat",
                tenant + "::" + branch + "::category::search"
        );

        for (String cacheName : List.of("category-tree", "category-flat", "category-search")) {

            var cache = cacheManager.getCache(cacheName);
            if (cache == null) continue;

            cache.invalidate(); // OR iterate keys if using advanced cache
        }
    }
}