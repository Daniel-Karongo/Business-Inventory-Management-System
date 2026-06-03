package com.IntegrityTechnologies.business_manager.config.caffeine;

import com.github.benmanes.caffeine.cache.Caffeine;
import org.springframework.cache.CacheManager;
import org.springframework.cache.caffeine.CaffeineCacheManager;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.cache.annotation.EnableCaching;

import java.time.Duration;

@Configuration
@EnableCaching
public class CacheConfiguration {

    @Bean
    public CacheManager cacheManager() {
        CaffeineCacheManager manager = new CaffeineCacheManager();

        /* =========================
           USERS
        ========================= */

        manager.registerCustomCache("user-by-identifier",
                Caffeine.newBuilder()
                        .maximumSize(20_000)
                        .expireAfterWrite(Duration.ofMinutes(10))
                        .recordStats()
                        .build()
        );

        manager.registerCustomCache("users-page",
                Caffeine.newBuilder()
                        .maximumSize(10_000)
                        .expireAfterWrite(Duration.ofMinutes(5))
                        .recordStats()
                        .build()
        );

        /* =========================
           ROLES  (rarely change)
        ========================= */
        manager.registerCustomCache("roles",
                Caffeine.newBuilder()
                        .maximumSize(1_000)
                        .expireAfterWrite(Duration.ofHours(6))
                        .recordStats()
                        .build()
        );


        /* =========================
           TENANTS (very stable)
        ========================= */
        manager.registerCustomCache("tenants",
                Caffeine.newBuilder()
                        .maximumSize(1_000)
                        .expireAfterWrite(Duration.ofHours(12))
                        .recordStats()
                        .build()
        );

        /* =========================
           FINANCIAL (expensive queries)
        ========================= */
        manager.registerCustomCache("trialBalance",
                Caffeine.newBuilder()
                        .maximumSize(5_000)
                        .expireAfterWrite(Duration.ofMinutes(10))
                        .recordStats()
                        .build()
        );

        manager.registerCustomCache("profitLoss",
                Caffeine.newBuilder()
                        .maximumSize(5_000)
                        .expireAfterWrite(Duration.ofMinutes(10))
                        .recordStats()
                        .build()
        );

        manager.registerCustomCache("balanceSheet",
                Caffeine.newBuilder()
                        .maximumSize(5_000)
                        .expireAfterWrite(Duration.ofMinutes(10))
                        .recordStats()
                        .build()
        );

        manager.registerCustomCache("cashFlow",
                Caffeine.newBuilder()
                        .maximumSize(5_000)
                        .expireAfterWrite(Duration.ofMinutes(10))
                        .recordStats()
                        .build()
        );

        manager.registerCustomCache("accountsReceivable",
                Caffeine.newBuilder()
                        .maximumSize(5_000)
                        .expireAfterWrite(Duration.ofMinutes(5))
                        .recordStats()
                        .build()
        );

        manager.registerCustomCache("accountsPayable",
                Caffeine.newBuilder()
                        .maximumSize(5_000)
                        .expireAfterWrite(Duration.ofMinutes(5))
                        .recordStats()
                        .build()
        );

        /* =========================
           CATEGORY (tree-heavy, stable)
        ========================= */
        manager.registerCustomCache("category-tree",
                Caffeine.newBuilder()
                        .maximumSize(5_000)
                        .expireAfterWrite(Duration.ofHours(1))
                        .recordStats()
                        .build()
        );

        manager.registerCustomCache("category-flat",
                Caffeine.newBuilder()
                        .maximumSize(5_000)
                        .expireAfterWrite(Duration.ofHours(1))
                        .recordStats()
                        .build()
        );

        manager.registerCustomCache("category-search",
                Caffeine.newBuilder()
                        .maximumSize(10_000)
                        .expireAfterWrite(Duration.ofMinutes(10))
                        .recordStats()
                        .build()
        );

        /* =========================
           PRODUCT / STOCK
        ========================= */
        manager.registerCustomCache("variant-search",
                Caffeine.newBuilder()
                        .maximumSize(10_000)
                        .expireAfterWrite(Duration.ofMinutes(5))
                        .recordStats()
                        .build()
        );

        manager.registerCustomCache("barcode-scan",
                Caffeine.newBuilder()
                        .maximumSize(20_000)
                        .expireAfterWrite(Duration.ofSeconds(30))
                        .recordStats()
                        .build()
        );

        manager.registerCustomCache("packaging",
                Caffeine.newBuilder()
                        .maximumSize(10_000)
                        .expireAfterWrite(Duration.ofMinutes(15))
                        .recordStats()
                        .build()
        );

        manager.registerCustomCache("pricing-preview",
                Caffeine.newBuilder()
                        .maximumSize(20_000)
                        .expireAfterWrite(Duration.ofMinutes(2))
                        .recordStats()
                        .build()
        );

        return manager;
    }
}