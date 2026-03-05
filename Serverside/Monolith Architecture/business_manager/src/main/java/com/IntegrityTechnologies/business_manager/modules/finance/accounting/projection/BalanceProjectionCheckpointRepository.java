package com.IntegrityTechnologies.business_manager.modules.finance.accounting.projection;

import org.springframework.data.jpa.repository.JpaRepository;

import java.util.UUID;

public interface BalanceProjectionCheckpointRepository
        extends JpaRepository<BalanceProjectionCheckpoint, UUID> {

    boolean existsByJournalId(UUID journalId);
}