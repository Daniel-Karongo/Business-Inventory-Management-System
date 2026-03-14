package com.IntegrityTechnologies.business_manager.modules.finance.tax.repository;

import com.IntegrityTechnologies.business_manager.modules.finance.tax.domain.VatFiling;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;
import java.util.UUID;

public interface VatFilingRepository
        extends JpaRepository<VatFiling, UUID> {

    boolean existsByTenantIdAndPeriod_IdAndBranchId(
            UUID tenantId,
            UUID periodId,
            UUID branchId
    );

    Page<VatFiling> findByTenantIdAndBranchId(
            UUID tenantId,
            UUID branchId,
            Pageable pageable
    );

    Optional<VatFiling>
    findByTenantIdAndPeriod_IdAndBranchId(
            UUID tenantId,
            UUID periodId,
            UUID branchId
    );

    Optional<VatFiling>
    findByTenantIdAndId(
            UUID tenantId,
            UUID id
    );
}