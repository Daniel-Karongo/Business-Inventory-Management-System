package com.IntegrityTechnologies.business_manager.modules.communication.notification.base;

import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;
import java.util.UUID;

public interface BranchNotificationSettingsRepository
        extends JpaRepository<BranchNotificationSettings, UUID> {

    Optional<BranchNotificationSettings> findByTenantIdAndBranchIdAndDeletedFalse(
            UUID tenantId,
            UUID branchId
    );
}