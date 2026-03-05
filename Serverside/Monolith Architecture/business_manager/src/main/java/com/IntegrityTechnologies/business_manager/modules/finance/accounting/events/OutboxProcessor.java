package com.IntegrityTechnologies.business_manager.modules.finance.accounting.events;

import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.List;

@Service
@RequiredArgsConstructor
public class OutboxProcessor {

    private final EventOutboxRepository outboxRepo;
    private final ApplicationEventPublisher publisher;
    private final ObjectMapper mapper;

    @Transactional
    public void processEvents() {

        List<EventOutbox> events =
                outboxRepo.lockNextBatch(PageRequest.of(0, 100));

        for (EventOutbox e : events) {

            try {

                if ("JOURNAL_POSTED".equals(e.getEventType())) {

                    JournalPostedEvent event =
                            mapper.readValue(
                                    e.getPayload(),
                                    JournalPostedEvent.class
                            );
                    outboxRepo.flush();
                    publisher.publishEvent(event);
                }

                e.setProcessed(true);
                e.setProcessedAt(LocalDateTime.now());

            } catch (Exception ex) {
                // leave event unprocessed for retry
            }
        }

        outboxRepo.saveAll(events);
    }
}