package com.IntegrityTechnologies.business_manager.modules.finance.accounting.events;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.context.TenantContext;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class OutboxEventWriter {

    private final EventOutboxRepository outboxRepo;
    private final ObjectMapper mapper;

    public void write(String eventType, UUID branchId, Object payload) {

        try {

            EventOutbox event = new EventOutbox();

            event.setTenantId(TenantContext.getTenantId());
            event.setBranchId(branchId);
            event.setEventType(eventType);
            event.setPayload(mapper.writeValueAsString(payload));
            event.setCreatedAt(LocalDateTime.now());

            outboxRepo.save(event);

        } catch (Exception e) {

            throw new RuntimeException(
                    "Failed to serialize outbox event",
                    e
            );
        }
    }
}