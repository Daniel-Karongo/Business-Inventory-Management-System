package com.IntegrityTechnologies.business_manager.config.caffeine;

import com.github.benmanes.caffeine.cache.Caffeine;
import org.springframework.cache.CacheManager;
import org.springframework.cache.annotation.EnableCaching;
import org.springframework.cache.caffeine.CaffeineCacheManager;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.time.Duration;

@Configuration
@EnableCaching
public class CacheConfiguration {

    @Bean
    public CacheManager cacheManager() {

        CaffeineCacheManager manager =
                new CaffeineCacheManager(

                        // USER MODULE
                        "users",
                        "roles",
                        "departments",

                        // TENANT MODULE
                        "tenants",

                        // FINANCIAL MODULE
                        "trialBalance",
                        "profitLoss",
                        "balanceSheet",
                        "cashFlow",
                        "accountsReceivable",
                        "accountsPayable",

                        // STOCK MODULE
                        "barcode-scan"
                );

        manager.setCaffeine(
                Caffeine.newBuilder()

                        // Prevent memory explosion
                        .maximumSize(50_000)

                        // Good default TTL
                        .expireAfterWrite(Duration.ofMinutes(30))

                        // Enables performance metrics
                        .recordStats()
        );

        return manager;
    }
}