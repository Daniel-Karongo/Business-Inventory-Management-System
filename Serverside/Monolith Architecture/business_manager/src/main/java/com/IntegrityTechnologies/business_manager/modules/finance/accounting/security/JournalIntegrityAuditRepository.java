package com.IntegrityTechnologies.business_manager.modules.finance.accounting.security;

import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;
import java.util.UUID;

public interface JournalIntegrityAuditRepository
        extends JpaRepository<JournalIntegrityAudit, UUID> {

    Optional<JournalIntegrityAudit> findTopByBranchIdOrderByVerifiedAtDesc(UUID branchId);
}