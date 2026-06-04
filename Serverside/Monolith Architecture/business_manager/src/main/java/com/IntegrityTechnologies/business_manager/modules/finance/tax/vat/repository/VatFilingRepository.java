package com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.repository;

import com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.model.VatFiling;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.model.enums.VatFilingStatus;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface VatFilingRepository
        extends JpaRepository<VatFiling, UUID> {

    Optional<VatFiling>
    findTopByTenantIdAndBranchIdOrderByFiledAtDesc(
            UUID tenantId,
            UUID branchId
    );

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

    Page<VatFiling> findByTenantIdAndBranchIdOrderByFiledAtDesc(
            UUID tenantId,
            UUID branchId,
            Pageable pageable
    );

    List<VatFiling> findByTenantIdAndBranchId(
            UUID tenantId,
            UUID branchId
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