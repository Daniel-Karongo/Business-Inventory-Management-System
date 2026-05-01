package com.IntegrityTechnologies.business_manager.config.caffeine;

import lombok.RequiredArgsConstructor;
import org.springframework.cache.Cache;
import org.springframework.cache.CacheManager;
import org.springframework.cache.concurrent.ConcurrentMapCache;
import org.springframework.stereotype.Component;

import java.util.UUID;

@Component
@RequiredArgsConstructor
public class CacheInvalidationService {

    private final CacheManager cacheManager;

    /* ============================================================
       LOW LEVEL HELPERS
    ============================================================ */

    private void evict(String cacheName, String key) {
        Cache cache = cacheManager.getCache(cacheName);
        if (cache != null) {
            cache.evict(key);
        }
    }

    private void evictByPrefix(String cacheName, String prefix) {
        Cache cache = cacheManager.getCache(cacheName);
        if (cache == null) return;

        Object nativeCache = cache.getNativeCache();

        if (nativeCache instanceof com.github.benmanes.caffeine.cache.Cache<?, ?> caffeineCache) {
            caffeineCache.asMap().keySet()
                    .removeIf(k -> k.toString().startsWith(prefix));
        }
    }

    /* ============================================================
       PACKAGING
    ============================================================ */

    public void evictPackaging(UUID tenantId, UUID variantId) {
        evict("packaging", CacheKeys.packaging(tenantId, variantId));
    }

    /* ============================================================
       PRICING
    ============================================================ */

    public void evictPricingByVariant(UUID tenantId, UUID variantId) {
        String prefix = tenantId + "::pricing::" + variantId + "::";
        evictByPrefix("pricing-preview", prefix);
    }

    /* ============================================================
       VARIANT SEARCH
    ============================================================ */

    public void evictVariantSearch(UUID tenantId, UUID branchId) {
        String prefix = tenantId + "::variant-search::" + branchId + "::";
        evictByPrefix("variant-search", prefix);
    }

    /* ============================================================
       BARCODE
    ============================================================ */

    public void evictBarcode(UUID tenantId, UUID branchId) {
        String prefix = tenantId + "::barcode::" + branchId + "::";
        evictByPrefix("barcode-scan", prefix);
    }

    /* ============================================================
       CATEGORY
    ============================================================ */

    public void evictCategoryCaches(UUID tenantId, UUID branchId) {
        evict("category-tree", CacheKeys.categoryTree(tenantId, branchId));
        evict("category-flat", CacheKeys.categoryFlat(tenantId, branchId));

        String prefix = tenantId + "::category::search::" + branchId + "::";
        evictByPrefix("category-search", prefix);
    }

    /* ============================================================
       USERS
    ============================================================ */

    /* ============================================================
   USERS (SAFE + FAST)
============================================================ */
    public void evictUsers(UUID tenantId) {

        String userPrefix = tenantId + "::user::";
        evictByPrefix("user-by-identifier", userPrefix);

        String pagePrefix = tenantId + "::users::";
        evictByPrefix("users-page", pagePrefix);
    }
}