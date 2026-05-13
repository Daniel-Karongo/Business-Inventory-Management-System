package com.IntegrityTechnologies.business_manager.modules.communication.notification.email.repository;

import com.IntegrityTechnologies.business_manager.modules.communication.notification.email.model.BranchEmailSettings;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;
import java.util.UUID;

public interface BranchEmailSettingsRepository
        extends JpaRepository<BranchEmailSettings, UUID> {

    Optional<BranchEmailSettings>
    findByTenantIdAndBranchIdAndDeletedFalse(
            UUID tenantId,
            UUID branchId
    );

    Optional<BranchEmailSettings>
    findByTenantIdAndBranchIdAndActiveTrueAndDeletedFalse(
            UUID tenantId,
            UUID branchId
    );
}