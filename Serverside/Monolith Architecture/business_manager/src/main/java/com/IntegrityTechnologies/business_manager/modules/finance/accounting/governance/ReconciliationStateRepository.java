package com.IntegrityTechnologies.business_manager.modules.finance.accounting.governance;

import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;
import java.util.UUID;

public interface ReconciliationStateRepository
        extends JpaRepository<ReconciliationState, UUID> {
    Optional<ReconciliationState> findByBranchId(UUID branchId);
    Optional<ReconciliationState> findTopByOrderByLastRunAtDesc();
}