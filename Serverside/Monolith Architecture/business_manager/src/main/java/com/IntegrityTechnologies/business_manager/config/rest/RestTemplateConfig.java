package com.IntegrityTechnologies.business_manager.config.rest;

import org.springframework.boot.web.client.RestTemplateBuilder;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.client.BufferingClientHttpRequestFactory;
import org.springframework.http.client.SimpleClientHttpRequestFactory;
import org.springframework.web.client.RestTemplate;

import java.time.Duration;

@Configuration
public class RestTemplateConfig {

    @Bean
    public RestTemplate restTemplate(
            RestTemplateBuilder builder
    ) {

        SimpleClientHttpRequestFactory factory =
                new SimpleClientHttpRequestFactory();

        factory.setConnectTimeout(10_000);
        factory.setReadTimeout(30_000);

        return builder
                .setConnectTimeout(
                        Duration.ofSeconds(10)
                )
                .setReadTimeout(
                        Duration.ofSeconds(30)
                )
                .requestFactory(() ->
                        new BufferingClientHttpRequestFactory(factory)
                )
                .build();
    }
}