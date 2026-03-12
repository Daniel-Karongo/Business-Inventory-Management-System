package com.IntegrityTechnologies.business_manager.modules.person.entity.system;

import lombok.RequiredArgsConstructor;
import org.springframework.boot.ApplicationRunner;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
@RequiredArgsConstructor
public class SystemInitializer {

    private final SystemInitializationService initializationService;

    @Bean
    public ApplicationRunner initializeDefaults() {
        return args -> initializationService.initialize();
    }
}