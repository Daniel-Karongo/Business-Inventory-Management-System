package com.IntegrityTechnologies.business_manager.config.kafka;

import org.springframework.data.jpa.repository.*;
import org.springframework.data.repository.query.Param;

import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

public interface EventOutboxRepository
        extends JpaRepository<EventOutbox, UUID> {

    @Modifying
    @Query("""
        DELETE FROM EventOutbox e
        WHERE e.tenantId = :tenantId
          AND e.processed = true
          AND e.processedAt < :cutoff
    """)
    void deleteProcessedBefore(
            @Param("tenantId") UUID tenantId,
            @Param("cutoff") LocalDateTime cutoff
    );

    @Query(value = """
        SELECT *
        FROM event_outbox
        WHERE processed = false
          AND failed = false
        ORDER BY created_at
        LIMIT 200
        FOR UPDATE SKIP LOCKED
    """, nativeQuery = true)
    List<EventOutbox> fetchBatchGlobal();

    List<EventOutbox> findByFailedTrueAndTenantId(UUID tenantId);
}