package com.IntegrityTechnologies.business_manager.modules.finance.tax.repository;

import com.IntegrityTechnologies.business_manager.modules.finance.tax.domain.CorporateTaxFiling;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;
import java.util.UUID;

public interface CorporateTaxFilingRepository
        extends JpaRepository<CorporateTaxFiling, UUID> {

    boolean existsByPeriodIdAndBranchId(UUID periodId, UUID branchId);

    Optional<CorporateTaxFiling>
    findTopByBranchIdOrderByFiledAtDesc(UUID branchId);

    Page<CorporateTaxFiling> findByBranchId(UUID branchId, Pageable pageable);
}