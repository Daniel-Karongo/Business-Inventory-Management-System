package com.IntegrityTechnologies.business_manager.config.async;

import com.IntegrityTechnologies.business_manager.security.util.TenantAwareExecutor;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;

import java.util.concurrent.Executor;

@Configuration
public class ExecutorConfig {

    @Bean(name = "emailExecutor")
    public Executor emailExecutor() {

        ThreadPoolTaskExecutor base =
                new ThreadPoolTaskExecutor();

        base.setCorePoolSize(4);
        base.setMaxPoolSize(8);
        base.setQueueCapacity(200);
        base.setThreadNamePrefix("EmailSender-");

        base.initialize();

        return new TenantAwareExecutor(base);
    }
}