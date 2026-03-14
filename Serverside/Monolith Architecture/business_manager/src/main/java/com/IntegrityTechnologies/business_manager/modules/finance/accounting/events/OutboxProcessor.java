package com.IntegrityTechnologies.business_manager.modules.finance.accounting.events;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.context.TenantContext;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import jakarta.annotation.PostConstruct;
import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

@Slf4j
@Service
@RequiredArgsConstructor
public class OutboxProcessor {

    private final EventOutboxRepository outboxRepo;
    private final AccountingEventPublisher publisher;
    private final ObjectMapper mapper;

    private static final int WORKERS = 4;
    private static final int POLL_DELAY_MS = 200;

    private final ExecutorService executor =
            Executors.newFixedThreadPool(WORKERS);

    @Transactional
    public void processEvent(EventOutbox e) {

        try {

            TenantContext.setTenantId(e.getTenantId());

            Object event = deserialize(e);

            publisher.publish(
                    e.getEventType(),
                    event
            );

            e.setProcessed(true);
            e.setProcessedAt(LocalDateTime.now());

            outboxRepo.save(e);

        } finally {

            TenantContext.clear();
        }
    }
    @PostConstruct
    public void startWorkers() {

        for (int i = 0; i < WORKERS; i++) {
            executor.submit(this::runWorker);
        }

        log.info("OutboxProcessor started with {} workers", WORKERS);
    }

    private void runWorker() {

        while (true) {

            try {

                processBatch();

                Thread.sleep(POLL_DELAY_MS);

            } catch (Exception e) {

                log.error("Outbox worker error", e);

                try {
                    Thread.sleep(1000);
                } catch (InterruptedException ignored) {}
            }
        }
    }

    public void processBatch() {

        List<EventOutbox> events = outboxRepo.fetchBatchGlobal();

        if (events.isEmpty())
            return;

        for (EventOutbox e : events) {

            try {

                processEvent(e);

            } catch (Exception ex) {

                log.error(
                        "Failed to publish event {}",
                        e.getId(),
                        ex
                );
            }
        }
    }

    private Object deserialize(EventOutbox e) {

        try {

            return switch (e.getEventType()) {

                case "JOURNAL_POSTED" ->
                        mapper.readValue(
                                e.getPayload(),
                                JournalPostedEvent.class
                        );

                case "ACCOUNTING_PERIOD_CLOSED" ->
                        mapper.readValue(
                                e.getPayload(),
                                AccountingPeriodClosedEvent.class
                        );

                default ->
                        mapper.readTree(e.getPayload());
            };

        } catch (Exception ex) {

            throw new RuntimeException(
                    "Failed to deserialize outbox event " + e.getId(),
                    ex
            );
        }
    }
}