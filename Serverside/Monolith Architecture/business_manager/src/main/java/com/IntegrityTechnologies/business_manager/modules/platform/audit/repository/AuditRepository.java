package com.IntegrityTechnologies.business_manager.modules.platform.audit.repository;

import com.IntegrityTechnologies.business_manager.modules.platform.audit.entity.AuditLog;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.UUID;

public interface AuditRepository extends JpaRepository<AuditLog, UUID> {
}