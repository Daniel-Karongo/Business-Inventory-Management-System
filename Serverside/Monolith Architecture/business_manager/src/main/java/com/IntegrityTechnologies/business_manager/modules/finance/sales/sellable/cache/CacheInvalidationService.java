package com.IntegrityTechnologies.business_manager.modules.finance.sales.sellable.cache;

import lombok.RequiredArgsConstructor;
import org.springframework.cache.Cache;
import org.springframework.cache.CacheManager;
import org.springframework.cache.concurrent.ConcurrentMapCache;
import org.springframework.stereotype.Component;

import java.util.Map;
import java.util.UUID;

@Component
@RequiredArgsConstructor
public class CacheInvalidationService {

    private final CacheManager cacheManager;

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

            nativeCache.keySet().removeIf(key ->
                    key.toString().contains("::pricing::" + variantId + "::" + branchId)
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
}