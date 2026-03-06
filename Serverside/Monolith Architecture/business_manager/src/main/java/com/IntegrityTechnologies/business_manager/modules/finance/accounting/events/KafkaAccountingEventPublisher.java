package com.IntegrityTechnologies.business_manager.modules.finance.accounting.events;

import lombok.RequiredArgsConstructor;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.kafka.core.KafkaTemplate;
import org.springframework.stereotype.Component;

@ConditionalOnProperty(
        name = "spring.kafka.enabled",
        havingValue = "true"
)
@Component
@RequiredArgsConstructor
public class KafkaAccountingEventPublisher implements AccountingEventPublisher {

    private final KafkaTemplate<String, Object> kafkaTemplate;

    @Override
    public void publish(String eventType, Object event) {

        String topic = mapTopic(eventType);

        kafkaTemplate.send(topic, event);
    }

    private String mapTopic(String eventType) {

        return switch (eventType) {

            case "JOURNAL_POSTED" -> "journal-posted";

            default -> "accounting-events";
        };
    }
}