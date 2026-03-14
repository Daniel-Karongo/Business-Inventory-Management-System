package com.IntegrityTechnologies.business_manager.modules.finance.accounting.projection;

import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;
import java.util.UUID;

public interface ProjectionCheckpointRepository
        extends JpaRepository<ProjectionCheckpoint, UUID> {

    Optional<ProjectionCheckpoint> findByTenantIdAndProjectionNameAndBranchId(
            UUID tenantId,
            String projectionName,
            UUID branchId
    );
}