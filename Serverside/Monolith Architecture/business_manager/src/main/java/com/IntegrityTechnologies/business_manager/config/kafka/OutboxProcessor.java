package com.IntegrityTechnologies.business_manager.config.kafka;

import jakarta.annotation.PreDestroy;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.context.event.ApplicationReadyEvent;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

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
    private volatile boolean running = true;

    private final ExecutorService executor =
            new ThreadPoolExecutor(
                    WORKERS,
                    WORKERS,
                    60L,
                    TimeUnit.SECONDS,
                    new LinkedBlockingQueue<>(5000),
                    r -> {
                        Thread t = new Thread(r);
                        t.setName("outbox-worker");
                        t.setDaemon(true);
                        return t;
                    },
                    new ThreadPoolExecutor.CallerRunsPolicy()
            );

    @EventListener(
            ApplicationReadyEvent.class
    )
    public void startWorkers() {

        for (int i = 0; i < WORKERS; i++) {
            executor.submit(this::runWorker);
        }

        log.info("OutboxProcessor started with {} workers", WORKERS);
    }

    private void runWorker() {

        long delay = MIN_DELAY_MS;

        while (running) {

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
                } catch (InterruptedException ignored) {
                    Thread.currentThread().interrupt();
                    return;
                }
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

    @PreDestroy
    public void shutdown() {

        running = false;

        executor.shutdown();

        try {

            if (!executor.awaitTermination(30, TimeUnit.SECONDS)) {
                executor.shutdownNow();
            }

        } catch (InterruptedException ignored) {

            executor.shutdownNow();

            Thread.currentThread().interrupt();
        }
    }
}