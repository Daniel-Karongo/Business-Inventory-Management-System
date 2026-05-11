package com.IntegrityTechnologies.business_manager.modules.finance.accounting.engine;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.events.JournalPostedEvent;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.projection.BalanceProjectionConsumer;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Service;

import jakarta.annotation.PreDestroy;
import java.util.UUID;
import java.util.concurrent.*;

@Service
@RequiredArgsConstructor
@Slf4j
public class BranchLedgerWorkerService {

    private final BalanceProjectionConsumer projectionConsumer;

    private final ConcurrentHashMap<UUID, ExecutorService> workers =
            new ConcurrentHashMap<>();

    private ExecutorService worker(UUID branchId) {

        return workers.computeIfAbsent(
                branchId,
                id -> Executors.newSingleThreadExecutor()
        );
    }

    @EventListener
    public void onJournalPosted(JournalPostedEvent event) {

        worker(event.branchId())
                .submit(() -> {

                    try {

                        projectionConsumer.handleSpring(event);

                    } catch (Exception ex) {

                        log.error(
                                "Ledger worker failed for branch {} journal {}",
                                event.branchId(),
                                event.journalId(),
                                ex
                        );
                    }
                });
    }

    @PreDestroy
    public void shutdown() {

        workers.values().forEach(ExecutorService::shutdown);
    }
}