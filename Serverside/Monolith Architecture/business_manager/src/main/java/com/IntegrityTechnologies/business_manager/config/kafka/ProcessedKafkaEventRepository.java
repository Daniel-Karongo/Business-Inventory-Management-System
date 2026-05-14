package com.IntegrityTechnologies.business_manager.config.kafka;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.transaction.annotation.Transactional;

import java.util.UUID;

public interface ProcessedKafkaEventRepository
        extends JpaRepository<ProcessedKafkaEvent, UUID> {

    @Modifying
    @Transactional
    @Query(
            value = """
                    INSERT IGNORE INTO processed_kafka_events
                    (
                        id,
                        tenant_id,
                        event_id,
                        consumer,
                        created_at,
                        updated_at,
                        deleted,
                        version
                    )
                    VALUES
                    (
                        :id,
                        :tenantId,
                        :eventId,
                        :consumer,
                        NOW(),
                        NOW(),
                        false,
                        0
                    )
                    """,
            nativeQuery = true
    )
    int tryClaim(
            @Param("id") UUID id,
            @Param("tenantId") UUID tenantId,
            @Param("eventId") UUID eventId,
            @Param("consumer") String consumer
    );
}