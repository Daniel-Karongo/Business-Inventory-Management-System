package com.IntegrityTechnologies.business_manager.modules.finance.tax.repository;

import com.IntegrityTechnologies.business_manager.modules.finance.tax.domain.VatFiling;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;
import java.util.UUID;

public interface VatFilingRepository extends JpaRepository<VatFiling, UUID> {

    boolean existsByPeriod_IdAndBranchId(UUID periodId, UUID branchId);
    Page<VatFiling> findByBranchId(UUID branchId, Pageable pageable);
    Optional<VatFiling> findByPeriod_IdAndBranchId(UUID periodId, UUID branchId);
}