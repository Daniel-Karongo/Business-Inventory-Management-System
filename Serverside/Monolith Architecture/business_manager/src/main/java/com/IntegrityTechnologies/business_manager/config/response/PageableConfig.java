package com.IntegrityTechnologies.business_manager.config.response;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.web.config.PageableHandlerMethodArgumentResolverCustomizer;

@Configuration
public class PageableConfig {

    @Bean
    public PageableHandlerMethodArgumentResolverCustomizer customizePageable() {
        return resolver -> resolver.setMaxPageSize(200);
    }
}