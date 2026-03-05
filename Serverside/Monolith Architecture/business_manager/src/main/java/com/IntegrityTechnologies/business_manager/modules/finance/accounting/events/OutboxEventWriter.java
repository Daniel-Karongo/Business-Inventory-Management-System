package com.IntegrityTechnologies.business_manager.modules.finance.accounting.events;

import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;

@Service
@RequiredArgsConstructor
public class OutboxEventWriter {

    private final EventOutboxRepository outboxRepo;
    private final ObjectMapper mapper;

    public void write(String eventType, Object payload) {

        try {

            EventOutbox event = new EventOutbox();
            event.setEventType(eventType);
            event.setPayload(mapper.writeValueAsString(payload));
            event.setCreatedAt(LocalDateTime.now());

            outboxRepo.save(event);

        } catch (Exception e) {
            throw new RuntimeException("Failed to serialize outbox event", e);
        }
    }
}