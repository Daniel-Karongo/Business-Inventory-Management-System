package com.IntegrityTechnologies.business_manager.security.acl.audit;

import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.UUID;

public interface AclAuditLogRepository
        extends JpaRepository<AclAuditLog, UUID> {

    List<AclAuditLog> findByEntityTypeAndEntityIdOrderByCreatedAtDesc(
            String entityType,
            String entityId
    );
}