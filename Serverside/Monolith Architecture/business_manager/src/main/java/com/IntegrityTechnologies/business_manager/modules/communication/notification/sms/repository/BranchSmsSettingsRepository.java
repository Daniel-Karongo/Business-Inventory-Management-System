package com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.repository;

import com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.model.BranchSmsSettings;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;
import java.util.UUID;

public interface BranchSmsSettingsRepository
        extends JpaRepository<BranchSmsSettings, UUID> {

    Optional<BranchSmsSettings>
    findByTenantIdAndBranchIdAndDeletedFalse(
            UUID tenantId,
            UUID branchId
    );

    Optional<BranchSmsSettings>
    findByTenantIdAndBranchIdAndActiveTrueAndDeletedFalse(
            UUID tenantId,
            UUID branchId
    );
}