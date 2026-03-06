package com.IntegrityTechnologies.business_manager.modules.finance.accounting.events;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.kafka.core.KafkaTemplate;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class KafkaEventPublisher {

    private final KafkaTemplate<String, String> kafkaTemplate;
    private final ApplicationEventPublisher springPublisher;
    private final ObjectMapper mapper;

    public void publish(String eventType, String payload) {

        String topic = mapTopic(eventType);

        try {

            JsonNode node = mapper.readTree(payload);
            String key = node.has("branchId")
                    ? node.get("branchId").asText()
                    : null;

            if (key != null) {
                kafkaTemplate.send(topic, key, payload);
            } else {
                kafkaTemplate.send(topic, payload);
            }

        } catch (Exception kafkaFailure) {

            try {

                // fallback to Spring event
                JournalPostedEvent event =
                        mapper.readValue(payload, JournalPostedEvent.class);

                springPublisher.publishEvent(event);

            } catch (Exception ignored) {
            }
        }
    }

    private String mapTopic(String eventType) {

        return switch (eventType) {

            case "JOURNAL_POSTED" -> "journal-posted";

            default -> "accounting-events";
        };
    }
}