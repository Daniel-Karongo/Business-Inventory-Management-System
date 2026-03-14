package com.IntegrityTechnologies.business_manager.modules.finance.tax.repository;

import com.IntegrityTechnologies.business_manager.modules.finance.tax.domain.TaxSystemState;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;
import java.util.UUID;

public interface TaxSystemStateRepository
        extends JpaRepository<TaxSystemState, UUID> {

    Optional<TaxSystemState>
    findByTenantIdAndBranchId(
            UUID tenantId,
            UUID branchId
    );
}