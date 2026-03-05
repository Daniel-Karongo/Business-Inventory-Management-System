package com.IntegrityTechnologies.business_manager.modules.finance.accounting.events;

import jakarta.persistence.LockModeType;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Lock;
import org.springframework.data.jpa.repository.Query;

import java.util.List;
import java.util.UUID;

public interface EventOutboxRepository
        extends JpaRepository<EventOutbox, UUID> {

    List<EventOutbox> findTop100ByProcessedFalseOrderByCreatedAtAsc();

    @Lock(LockModeType.PESSIMISTIC_WRITE)
    @Query("""
        SELECT e
        FROM EventOutbox e
        WHERE e.processed = false
        ORDER BY e.createdAt
    """)
    List<EventOutbox> lockNextBatch(Pageable pageable);
}