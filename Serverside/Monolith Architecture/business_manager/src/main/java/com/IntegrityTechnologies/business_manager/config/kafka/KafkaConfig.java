package com.IntegrityTechnologies.business_manager.config.kafka;

import org.apache.kafka.clients.admin.NewTopic;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@ConditionalOnProperty(
        name = "spring.kafka.enabled",
        havingValue = "true",
        matchIfMissing = false
)
@Configuration
public class KafkaConfig {

    @Bean
    public NewTopic journalPostedTopic() {
        return new NewTopic("journal-posted", 12, (short) 3);
    }

    @Bean
    public NewTopic accountingEventsTopic() {
        return new NewTopic("accounting-events", 3, (short) 1);
    }

    @Bean
    public NewTopic accountingPeriodClosedTopic() {
        return new NewTopic("accounting-period-closed", 6, (short) 2);
    }

    @Bean
    public NewTopic variantBarcodeTopic() {
        return new NewTopic("variant-barcode", 3, (short) 1);
    }

    @Bean
    public NewTopic variantImageTopic() {
        return new NewTopic("variant-image", 3, (short) 1);
    }

    @Bean
    public NewTopic variantPdfTopic() {
        return new NewTopic("variant-pdf", 3, (short) 1);
    }
}