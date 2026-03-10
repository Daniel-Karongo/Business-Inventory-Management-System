package com.IntegrityTechnologies.business_manager.modules.platform.identity.repository;

import com.IntegrityTechnologies.business_manager.modules.platform.identity.entity.PlatformUserAudit;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.UUID;

public interface PlatformUserAuditRepository
        extends JpaRepository<PlatformUserAudit, UUID> {
}