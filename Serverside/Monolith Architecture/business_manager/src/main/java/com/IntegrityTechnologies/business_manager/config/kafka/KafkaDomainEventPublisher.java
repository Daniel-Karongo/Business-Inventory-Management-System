package com.IntegrityTechnologies.business_manager.config.kafka;

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
public class KafkaDomainEventPublisher implements DomainEventPublisher {

    private final KafkaTemplate<String, Object> kafkaTemplate;

    @Override
    public void publish(String eventType, Object event) {

        String topic = mapTopic(eventType);

        kafkaTemplate.send(topic, event);
    }

    private String mapTopic(String eventType) {

        return switch (eventType) {

            // ACCOUNTING
            case "JOURNAL_POSTED" -> "journal-posted";
            case "ACCOUNTING_PERIOD_CLOSED" -> "accounting-period-closed";

            // VARIANT
            case "VARIANT_BARCODE_REQUESTED" -> "variant-barcode";
            case "VARIANT_IMAGE_UPLOAD_REQUESTED" -> "variant-image";
            case "VARIANT_BARCODE_PDF_REQUESTED" -> "variant-pdf";

            // SAFETY (DO NOT SILENTLY DROP)
            default -> throw new IllegalArgumentException(
                    "No topic mapping for event type: " + eventType
            );
        };
    }
}