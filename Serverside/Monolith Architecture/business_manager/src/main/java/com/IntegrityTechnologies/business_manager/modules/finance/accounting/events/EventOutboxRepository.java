package com.IntegrityTechnologies.business_manager.modules.finance.accounting.events;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;

import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

public interface EventOutboxRepository
        extends JpaRepository<EventOutbox, UUID> {

    @Query(value = """
        SELECT *
        FROM event_outbox
        WHERE processed = false
        ORDER BY created_at
        LIMIT 200
        FOR UPDATE SKIP LOCKED
    """, nativeQuery = true)
    List<EventOutbox> fetchBatch();

    @Modifying
    @Query("""
        DELETE FROM EventOutbox e
        WHERE e.processed = true
        AND e.processedAt < :cutoff
    """)
    void deleteProcessedBefore(LocalDateTime cutoff);
}