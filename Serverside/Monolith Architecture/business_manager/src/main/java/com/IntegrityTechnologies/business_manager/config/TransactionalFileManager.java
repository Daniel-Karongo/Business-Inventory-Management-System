package com.IntegrityTechnologies.business_manager.config;

import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.transaction.support.TransactionSynchronization;
import org.springframework.transaction.support.TransactionSynchronizationAdapter;
import org.springframework.transaction.support.TransactionSynchronizationManager;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

/**
 * Tracks files written during the current transaction and deletes them if the transaction rolls back.
 *
 * Usage:
 *   transactionalFileManager.track(savedPath);
 * The manager will register a TransactionSynchronization automatically (first time).
 */
@Component
@Slf4j
public class TransactionalFileManager implements TransactionSynchronization {

    private final List<Path> written = new ArrayList<>();
    private boolean registered = false;

    /**
     * Track a path as "written" during the current transaction. If a transaction is active,
     * ensures this object is registered for rollback cleanup.
     */
    public synchronized void track(Path path) {
        if (path == null) return;
        written.add(path);
        if (TransactionSynchronizationManager.isSynchronizationActive() && !registered) {
            TransactionSynchronizationManager.registerSynchronization(this);
            registered = true;
        }
    }

    // TransactionSynchronization callbacks
    @Override
    public void afterCompletion(int status) {
        // If transaction did not commit, cleanup written files
        if (status != STATUS_COMMITTED) {
            for (Path p : written) {
                try {
                    Files.deleteIfExists(p);
                    log.info("Deleted file after rollback: {}", p);
                } catch (IOException e) {
                    log.warn("Failed to delete file after rollback: {}", p, e);
                }
            }
        }
        // Reset state
        written.clear();
        registered = false;
    }

    public void runAfterCommit(Runnable action) {
        if (TransactionSynchronizationManager.isSynchronizationActive()) {
            TransactionSynchronizationManager.registerSynchronization(new TransactionSynchronizationAdapter() {
                @Override
                public void afterCommit() {
                    try {
                        log.debug("Running post-commit action...");
                        action.run();
                    } catch (Exception e) {
                        log.error("Error during post-commit file cleanup: {}", e.getMessage(), e);
                    }
                }
            });
        } else {
            log.warn("No active transaction. Running action immediately...");
            action.run();
        }
    }

    // other methods no-op
    @Override public void suspend() {}
    @Override public void resume() {}
    @Override public void flush() {}
    @Override public void beforeCommit(boolean readOnly) {}
    @Override public void beforeCompletion() {}
    @Override public void afterCommit() {}
}