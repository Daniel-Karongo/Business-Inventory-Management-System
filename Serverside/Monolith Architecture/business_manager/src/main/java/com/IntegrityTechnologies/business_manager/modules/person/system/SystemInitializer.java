package com.IntegrityTechnologies.business_manager.modules.person.system;

import lombok.RequiredArgsConstructor;
import org.springframework.boot.ApplicationRunner;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.DependsOn;
import org.springframework.core.annotation.Order;

@Configuration
@RequiredArgsConstructor
public class SystemInitializer {

    private final SystemInitializationService initializationService;

    @Bean
    @Order(2)
    public ApplicationRunner initializeDefaults() {
        return args -> initializationService.initialize();
    }
}