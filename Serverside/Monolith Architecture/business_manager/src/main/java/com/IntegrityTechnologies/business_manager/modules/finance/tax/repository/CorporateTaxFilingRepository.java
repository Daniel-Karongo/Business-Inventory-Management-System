package com.IntegrityTechnologies.business_manager.modules.finance.tax.repository;

import com.IntegrityTechnologies.business_manager.modules.finance.tax.domain.CorporateTaxFiling;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;
import java.util.UUID;

public interface CorporateTaxFilingRepository
        extends JpaRepository<CorporateTaxFiling, UUID> {

    boolean existsByTenantIdAndPeriodIdAndBranchId(
            UUID tenantId,
            UUID periodId,
            UUID branchId
    );

    Optional<CorporateTaxFiling>
    findTopByTenantIdAndBranchIdOrderByFiledAtDesc(
            UUID tenantId,
            UUID branchId
    );

    Page<CorporateTaxFiling> findByTenantIdAndBranchId(
            UUID tenantId,
            UUID branchId,
            Pageable pageable
    );

    Optional<CorporateTaxFiling>
    findByTenantIdAndId(
            UUID tenantId,
            UUID id
    );
}