package com.IntegrityTechnologies.business_manager.modules.finance.accounting.events;

import org.springframework.data.jpa.repository.*;
import org.springframework.data.repository.query.Param;

import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

public interface EventOutboxRepository
        extends JpaRepository<EventOutbox, UUID> {

    @Query(value = """
        SELECT *
        FROM event_outbox
        WHERE tenant_id = :tenantId
          AND processed = false
        ORDER BY created_at
        LIMIT 200
        FOR UPDATE SKIP LOCKED
    """, nativeQuery = true)
    List<EventOutbox> fetchBatch(@Param("tenantId") UUID tenantId);

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
        ORDER BY created_at
        LIMIT 200
        FOR UPDATE SKIP LOCKED
    """, nativeQuery = true)
    List<EventOutbox> fetchBatchGlobal();
}