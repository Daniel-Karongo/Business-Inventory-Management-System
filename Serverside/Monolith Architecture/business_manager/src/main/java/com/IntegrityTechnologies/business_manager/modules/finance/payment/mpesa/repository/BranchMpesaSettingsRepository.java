package com.IntegrityTechnologies.business_manager.modules.finance.payment.mpesa.repository;

import com.IntegrityTechnologies.business_manager.modules.finance.payment.mpesa.model.BranchMpesaSettings;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;
import java.util.UUID;

public interface BranchMpesaSettingsRepository
        extends JpaRepository<BranchMpesaSettings, UUID> {

    Optional<BranchMpesaSettings>
    findByTenantIdAndBranchIdAndDeletedFalse(
            UUID tenantId,
            UUID branchId
    );

    Optional<BranchMpesaSettings>
    findByTenantIdAndBranchIdAndActiveTrueAndDeletedFalse(
            UUID tenantId,
            UUID branchId
    );
}