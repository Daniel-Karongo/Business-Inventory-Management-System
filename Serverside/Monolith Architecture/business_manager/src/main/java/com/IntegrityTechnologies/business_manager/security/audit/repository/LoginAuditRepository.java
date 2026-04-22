package com.IntegrityTechnologies.business_manager.security.audit.repository;

import com.IntegrityTechnologies.business_manager.security.audit.model.LoginAudit;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.UUID;

public interface LoginAuditRepository extends JpaRepository<LoginAudit, UUID> {

    List<LoginAudit> findByTenantIdAndFingerprint(
            UUID tenantId,
            String fingerprint
    );

    long countByTenantIdAndFingerprintAndReason(
            UUID tenantId,
            String fingerprint,
            String reason
    );
}