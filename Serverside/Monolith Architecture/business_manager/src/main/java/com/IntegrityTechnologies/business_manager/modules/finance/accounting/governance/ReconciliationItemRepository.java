package com.IntegrityTechnologies.business_manager.modules.finance.accounting.governance;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.UUID;

public interface ReconciliationItemRepository
        extends JpaRepository<ReconciliationItem, UUID> {

    Page<ReconciliationItem> findByRun_Id(
            UUID runId,
            Pageable pageable
    );

    Page<ReconciliationItem> findByRun_IdAndConsistentFalse(
            UUID runId,
            Pageable pageable
    );
}