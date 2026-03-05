package com.IntegrityTechnologies.business_manager.modules.finance.accounting.governance;

import org.springframework.data.jpa.repository.JpaRepository;

import java.util.UUID;

public interface ReconciliationRunRepository
        extends JpaRepository<ReconciliationRun, UUID> {
}
