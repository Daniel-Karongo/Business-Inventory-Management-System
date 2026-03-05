package com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.BranchAccountingSettings;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;
import java.util.UUID;

public interface BranchAccountingSettingsRepository
        extends JpaRepository<BranchAccountingSettings, UUID> {

    Optional<BranchAccountingSettings> findByBranchId(UUID branchId);

    boolean existsByBranchId(UUID branchId);
}