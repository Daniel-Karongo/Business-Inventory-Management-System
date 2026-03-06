package com.IntegrityTechnologies.business_manager.modules.finance.accounting.config;

import com.github.benmanes.caffeine.cache.Caffeine;
import org.springframework.cache.CacheManager;
import org.springframework.cache.annotation.EnableCaching;
import org.springframework.cache.caffeine.CaffeineCacheManager;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.time.Duration;

@Configuration
@EnableCaching
public class FinancialCacheConfiguration {

    @Bean
    public CacheManager financialCacheManager() {

        CaffeineCacheManager manager =
                new CaffeineCacheManager(
                        "trialBalance",
                        "profitLoss",
                        "balanceSheet",
                        "cashFlow",
                        "accountsReceivable",
                        "accountsPayable"
                );

        manager.setCaffeine(
                Caffeine.newBuilder()

                        // Prevent memory explosion
                        .maximumSize(10_000)

                        // Financial reports rarely need to be recalculated instantly
                        .expireAfterWrite(Duration.ofMinutes(15))

                        // Performance metrics
                        .recordStats()
        );

        return manager;
    }
}