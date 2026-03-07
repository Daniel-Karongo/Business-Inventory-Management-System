package com.IntegrityTechnologies.business_manager.modules.platform.tenant.config;

import com.github.benmanes.caffeine.cache.Caffeine;
import org.springframework.cache.CacheManager;
import org.springframework.cache.annotation.EnableCaching;
import org.springframework.cache.caffeine.CaffeineCacheManager;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.time.Duration;

@Configuration
@EnableCaching
public class TenantCacheConfiguration {

    @Bean
    public CacheManager tenantCacheManager() {

        CaffeineCacheManager manager =
                new CaffeineCacheManager("tenants");

        manager.setCaffeine(
                Caffeine.newBuilder()
                        .maximumSize(1000)
                        .expireAfterWrite(Duration.ofMinutes(30))
                        .recordStats()
        );

        return manager;
    }
}