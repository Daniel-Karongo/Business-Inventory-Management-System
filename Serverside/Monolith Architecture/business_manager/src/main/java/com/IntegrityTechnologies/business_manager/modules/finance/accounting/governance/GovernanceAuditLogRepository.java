package com.IntegrityTechnologies.business_manager.modules.finance.accounting.governance;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.UUID;

public interface GovernanceAuditLogRepository
        extends JpaRepository<GovernanceAuditLog, UUID> {
    Page<GovernanceAuditLog> findByBranchId(UUID branchId, Pageable pageable);
}