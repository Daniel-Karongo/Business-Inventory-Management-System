package com.IntegrityTechnologies.business_manager.modules.finance.accounting.governance;

import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;
import java.util.UUID;

public interface AccountingSystemStateRepository
        extends JpaRepository<AccountingSystemState, UUID> {

    Optional<AccountingSystemState>
    findByTenantIdAndBranchId(UUID tenantId, UUID branchId);

}