package com.IntegrityTechnologies.business_manager.config;

import org.apache.kafka.clients.admin.NewTopic;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class KafkaConfig {

    @Bean
    public NewTopic journalPostedTopic() {
        return new NewTopic("journal-posted", 6, (short) 1);
    }

    @Bean
    public NewTopic accountingEventsTopic() {
        return new NewTopic("accounting-events", 3, (short) 1);
    }
}