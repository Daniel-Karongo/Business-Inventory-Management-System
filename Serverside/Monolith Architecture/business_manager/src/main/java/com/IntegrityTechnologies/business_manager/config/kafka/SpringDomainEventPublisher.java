package com.IntegrityTechnologies.business_manager.config.kafka;

import lombok.RequiredArgsConstructor;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Component;

@ConditionalOnProperty(
        name = "spring.kafka.enabled",
        havingValue = "false",
        matchIfMissing = true
)
@Component
@RequiredArgsConstructor
public class SpringDomainEventPublisher implements DomainEventPublisher {

    private final ApplicationEventPublisher publisher;

    @Override
    public void publish(String eventType, Object event) {
        publisher.publishEvent(event);
    }
}