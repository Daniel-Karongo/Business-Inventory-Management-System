package com.IntegrityTechnologies.business_manager.config.files;

import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.transaction.support.TransactionSynchronization;
import org.springframework.transaction.support.TransactionSynchronizationManager;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

@Component
@Slf4j
public class TransactionalFileManager implements TransactionSynchronization {

    private static final ThreadLocal<List<Path>> WRITTEN =
            ThreadLocal.withInitial(ArrayList::new);

    private static final ThreadLocal<Boolean> REGISTERED =
            ThreadLocal.withInitial(() -> false);

    public void track(Path path) {

        if (path == null) return;

        WRITTEN.get().add(path);

        if (TransactionSynchronizationManager.isSynchronizationActive()
                && !REGISTERED.get()) {

            TransactionSynchronizationManager.registerSynchronization(this);
            REGISTERED.set(true);
        }
    }

    @Override
    public void afterCompletion(int status) {
        List<Path> written = WRITTEN.get();

        if (status != STATUS_COMMITTED) {
            for (Path p : written) {
                try {
                    deleteRecursivelySafe(p);
                    log.info("Deleted file after rollback: {}", p);
                } catch (Exception e) {
                    log.warn("Failed to delete file after rollback: {}", p, e);
                }
            }
        }

        written.clear();
        WRITTEN.remove();
        REGISTERED.remove();
    }

    private void deleteRecursivelySafe(Path path) throws IOException {
        if (path == null || !Files.exists(path)) return;

        Files.walk(path)
                .sorted(Comparator.reverseOrder())
                .forEach(p -> {
                    try {
                        Files.deleteIfExists(p);
                    } catch (IOException e) {
                        log.warn("Failed deleting: {}", p);
                    }
                });
    }

    public void runAfterCommit(Runnable action) {

        if (TransactionSynchronizationManager.isSynchronizationActive()) {

            TransactionSynchronizationManager.registerSynchronization(
                    new TransactionSynchronization() {

                        @Override
                        public void afterCommit() {

                            try {
                                action.run();
                            } catch (Exception e) {
                                log.error("Post-commit file action failed", e);
                            }
                        }
                    }
            );

        } else {
            action.run();
        }
    }

    @Override public void suspend() {}
    @Override public void resume() {}
    @Override public void flush() {}
    @Override public void beforeCommit(boolean readOnly) {}
    @Override public void beforeCompletion() {}
    @Override public void afterCommit() {}
}