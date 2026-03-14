package com.IntegrityTechnologies.business_manager.modules.finance.accounting.projection;

import org.springframework.data.jpa.repository.JpaRepository;

import java.util.UUID;

public interface ProcessedKafkaEventRepository
        extends JpaRepository<ProcessedKafkaEvent, UUID> {

    boolean existsByTenantIdAndEventId(UUID tenantId, UUID eventId);
}