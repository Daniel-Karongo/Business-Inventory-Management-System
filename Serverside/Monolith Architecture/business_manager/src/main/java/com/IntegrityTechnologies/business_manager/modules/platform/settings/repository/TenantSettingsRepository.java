package com.IntegrityTechnologies.business_manager.modules.platform.settings.repository;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.entity.TenantSettings;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.UUID;

public interface TenantSettingsRepository
        extends JpaRepository<TenantSettings, UUID> {
}