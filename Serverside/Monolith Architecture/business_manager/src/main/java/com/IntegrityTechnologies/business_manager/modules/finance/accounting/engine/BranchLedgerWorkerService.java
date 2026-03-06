package com.IntegrityTechnologies.business_manager.modules.finance.accounting.engine;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.events.JournalPostedEvent;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.projection.BalanceProjectionConsumer;
import lombok.RequiredArgsConstructor;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Service;

import jakarta.annotation.PreDestroy;

import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

@Service
@RequiredArgsConstructor
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
                .submit(() -> projectionConsumer.handleSpring(event));
    }

    @PreDestroy
    public void shutdown() {

        workers.values().forEach(ExecutorService::shutdown);
    }
}