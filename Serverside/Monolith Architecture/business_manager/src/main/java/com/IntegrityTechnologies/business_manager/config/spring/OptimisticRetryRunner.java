package com.IntegrityTechnologies.business_manager.config;

import jakarta.persistence.OptimisticLockException;
import org.hibernate.StaleObjectStateException;

import java.util.concurrent.Callable;

public class OptimisticRetryRunner {

    public static final int DEFAULT_MAX_RETRIES = 5;
    public static final long DEFAULT_SLEEP_MS = 50;

    public static <T> T runWithRetry(Callable<T> action) {
        return runWithRetry(action, DEFAULT_MAX_RETRIES, DEFAULT_SLEEP_MS);
    }

    public static <T> T runWithRetry(Callable<T> action, int maxRetries, long sleepMs) {
        int attempts = 0;
        while (true) {
            try {
                return action.call();
            } catch (StaleObjectStateException | OptimisticLockException e) {
                attempts++;
                if (attempts >= maxRetries) {
                    throw new RuntimeException("Concurrent update conflict after " + attempts + " attempts", e);
                }
                try {
                    Thread.sleep(sleepMs);
                } catch (InterruptedException ignored) {
                    Thread.currentThread().interrupt();
                }
            } catch (RuntimeException re) {
                // If underlying action already wrapped a checked exception into runtime, rethrow
                throw re;
            } catch (Exception ex) {
                throw new RuntimeException(ex);
            }
        }
    }
}