package com.IntegrityTechnologies.business_manager.modules.finance.accounting.events;

import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.PageRequest;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.List;

@Service
@RequiredArgsConstructor
public class OutboxProcessor {

    private final EventOutboxRepository outboxRepo;
    private final KafkaEventPublisher kafkaPublisher;

    private static final int BATCH_SIZE = 100;

    @Scheduled(fixedDelay = 1000)
    @Transactional
    public void processEvents() {

        List<EventOutbox> events =
                outboxRepo.lockNextBatch(PageRequest.of(0, BATCH_SIZE));

        for (EventOutbox e : events) {

            try {

                kafkaPublisher.publish(
                        e.getEventType(),
                        e.getPayload()
                );

                e.setProcessed(true);
                e.setProcessedAt(LocalDateTime.now());

            } catch (Exception ignored) {
            }
        }

        outboxRepo.saveAll(events);
    }
}