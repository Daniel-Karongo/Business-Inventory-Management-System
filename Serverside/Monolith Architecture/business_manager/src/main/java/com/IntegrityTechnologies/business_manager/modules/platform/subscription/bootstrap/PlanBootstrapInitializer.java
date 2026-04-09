package com.IntegrityTechnologies.business_manager.modules.platform.subscription.bootstrap;

import lombok.RequiredArgsConstructor;
import org.springframework.boot.ApplicationRunner;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
@RequiredArgsConstructor
public class PlanBootstrapInitializer {

    private final PlanBootstrapService planBootstrapService;
    @Bean
    public ApplicationRunner bootstrapPlans() {
        return args -> planBootstrapService.bootstrap();
    }
}
