package com.IntegrityTechnologies.business_manager.config.async;

import com.IntegrityTechnologies.business_manager.security.util.TenantAwareExecutor;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.annotation.EnableAsync;

import java.util.concurrent.Executor;
import java.util.concurrent.Executors;

@Configuration
@EnableAsync
public class AsyncConfig {

    @Bean(name = "tenantAwareExecutor")
    public Executor tenantAwareExecutor() {

        Executor base = Executors.newFixedThreadPool(
                Runtime.getRuntime().availableProcessors() * 2
        );

        return new TenantAwareExecutor(base);
    }
}