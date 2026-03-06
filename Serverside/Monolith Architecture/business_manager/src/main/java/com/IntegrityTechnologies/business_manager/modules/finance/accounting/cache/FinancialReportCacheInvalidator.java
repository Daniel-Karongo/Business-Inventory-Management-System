package com.IntegrityTechnologies.business_manager.modules.finance.accounting.cache;

import com.github.benmanes.caffeine.cache.Cache;
import lombok.RequiredArgsConstructor;
import org.springframework.cache.CacheManager;
import org.springframework.cache.caffeine.CaffeineCache;
import org.springframework.stereotype.Component;
import org.springframework.transaction.event.TransactionPhase;
import org.springframework.transaction.event.TransactionalEventListener;

@Component
@RequiredArgsConstructor
public class FinancialReportCacheInvalidator {

    private final CacheManager cacheManager;

    @TransactionalEventListener(phase = TransactionPhase.AFTER_COMMIT)
    public void onLedgerUpdate(AccountingLedgerUpdatedEvent event) {

        String branchPrefix = event.branchId().toString();

        evictBranch("trialBalance", branchPrefix);
        evictBranch("profitLoss", branchPrefix);
        evictBranch("balanceSheet", branchPrefix);
        evictBranch("cashFlow", branchPrefix);
        evictBranch("accountsReceivable", branchPrefix);
        evictBranch("accountsPayable", branchPrefix);
    }

    private void evictBranch(String cacheName, String branchPrefix) {

        var cache = cacheManager.getCache(cacheName);

        if (!(cache instanceof CaffeineCache caffeineCache)) {
            return;
        }

        Cache<Object,Object> nativeCache = caffeineCache.getNativeCache();

        nativeCache.asMap().keySet().removeIf(key ->
                key.toString().startsWith(branchPrefix)
        );
    }
}