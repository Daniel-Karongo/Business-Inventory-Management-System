package com.IntegrityTechnologies.business_manager.modules.finance.accounting.cache;

import com.github.benmanes.caffeine.cache.Cache;
import org.springframework.cache.CacheManager;
import org.springframework.cache.caffeine.CaffeineCache;
import org.springframework.stereotype.Component;
import org.springframework.transaction.event.TransactionPhase;
import org.springframework.transaction.event.TransactionalEventListener;

@Component
public class FinancialReportCacheInvalidator {

    private final CacheManager cacheManager;

    public FinancialReportCacheInvalidator(CacheManager cacheManager) {
        this.cacheManager = cacheManager;
    }

    @TransactionalEventListener(phase = TransactionPhase.AFTER_COMMIT)
    public void onLedgerUpdate(AccountingLedgerUpdatedEvent event) {

        String tenant = event.tenantId().toString();
        String branch = event.branchId().toString();

        evict("trialBalance", tenant, branch);
        evict("profitLoss", tenant, branch);
        evict("balanceSheet", tenant, branch);
        evict("cashFlow", tenant, branch);
        evict("accountsReceivable", tenant, branch);
        evict("accountsPayable", tenant, branch);
    }

    private void evict(String cacheName, String tenant, String branch) {

        var cache = cacheManager.getCache(cacheName);

        if (!(cache instanceof CaffeineCache caffeineCache)) return;

        Cache<Object, Object> nativeCache = caffeineCache.getNativeCache();

        nativeCache.asMap().keySet().removeIf(key -> {

            if (key == null) return false;

            String k = key.toString();

            return k.contains(tenant) && k.contains(branch);

        });
    }

    private void evictBranch(String cacheName, String branchPrefix) {

        var cache = cacheManager.getCache(cacheName);

        if (!(cache instanceof CaffeineCache caffeineCache)) return;

        Cache<Object, Object> nativeCache = caffeineCache.getNativeCache();

        nativeCache.asMap().keySet().removeIf(key ->
                key != null && key.toString().startsWith(branchPrefix)
        );
    }
}