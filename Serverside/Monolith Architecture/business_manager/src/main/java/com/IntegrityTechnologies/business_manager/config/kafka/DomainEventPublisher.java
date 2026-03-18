package com.IntegrityTechnologies.business_manager.config.kafka;

public interface DomainEventPublisher {

    void publish(String eventType, Object event);
}