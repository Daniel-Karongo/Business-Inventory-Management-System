package com.IntegrityTechnologies.business_manager.config.kafka;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import jakarta.annotation.PostConstruct;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

@Slf4j
@Service
@RequiredArgsConstructor
public class OutboxProcessor {

    private final EventOutboxRepository outboxRepo;
    private final OutboxEventExecutor executorService;

    private static final int WORKERS = 4;

    // 🔥 BACKOFF CONFIG
    private static final long MIN_DELAY_MS = 100;
    private static final long MAX_DELAY_MS = 5000;

    private final ExecutorService executor =
            Executors.newFixedThreadPool(WORKERS);

    @PostConstruct
    public void startWorkers() {

        for (int i = 0; i < WORKERS; i++) {
            executor.submit(this::runWorker);
        }

        log.info("OutboxProcessor started with {} workers", WORKERS);
    }

    private void runWorker() {

        long delay = MIN_DELAY_MS;

        while (true) {

            try {

                boolean processed = processBatch();

                if (processed) {
                    delay = MIN_DELAY_MS; // reset on work
                } else {
                    delay = Math.min(delay * 2, MAX_DELAY_MS); // backoff
                }

                Thread.sleep(delay);

            } catch (Exception e) {

                log.error("Outbox worker error", e);

                try {
                    Thread.sleep(MAX_DELAY_MS);
                } catch (InterruptedException ignored) {}
            }
        }
    }

    public boolean processBatch() {

        List<EventOutbox> events = outboxRepo.fetchBatchGlobal();

        if (events.size() == 200) {
            log.warn("Outbox backlog growing — consider scaling consumers");
        }

        if (events.isEmpty()) {
            return false;
        }

        for (EventOutbox e : events) {

            try {
                executorService.processEvent(e);
            } catch (Exception ignored) {
                // already logged
            }
        }

        return true;
    }
}