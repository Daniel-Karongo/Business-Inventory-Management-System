package com.IntegrityTechnologies.business_manager.modules.finance.tax.repository;

import com.IntegrityTechnologies.business_manager.modules.finance.tax.domain.TaxPeriod;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;
import java.util.UUID;

public interface TaxPeriodRepository
        extends JpaRepository<TaxPeriod, UUID> {

    Page<TaxPeriod> findByTenantIdAndBranchId(
            UUID tenantId,
            UUID branchId,
            Pageable pageable
    );

    Optional<TaxPeriod> findByTenantIdAndBranchIdAndClosedFalse(
            UUID tenantId,
            UUID branchId
    );

    Optional<TaxPeriod> findByTenantIdAndId(
            UUID tenantId,
            UUID id
    );
}