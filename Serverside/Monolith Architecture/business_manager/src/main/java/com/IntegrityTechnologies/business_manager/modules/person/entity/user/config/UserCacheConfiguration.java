package com.IntegrityTechnologies.business_manager.modules.person.entity.user.config;

import com.github.benmanes.caffeine.cache.Caffeine;
import org.springframework.cache.CacheManager;
import org.springframework.cache.annotation.EnableCaching;
import org.springframework.cache.caffeine.CaffeineCacheManager;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.time.Duration;

@Configuration
@EnableCaching
public class UserCacheConfiguration {

    @Bean
    public CacheManager userCacheManager() {

        CaffeineCacheManager manager =
                new CaffeineCacheManager(
                        "users",
                        "roles",
                        "departments"
                );

        manager.setCaffeine(
                Caffeine.newBuilder()
                        .maximumSize(50_000)
                        .expireAfterWrite(Duration.ofMinutes(30))
                        .recordStats()
        );

        return manager;
    }
}