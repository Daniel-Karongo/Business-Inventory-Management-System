package com.IntegrityTechnologies.business_manager.security.audit.repository;

import com.IntegrityTechnologies.business_manager.security.audit.model.LoginAudit;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

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

    @Query("""
                select la
                from LoginAudit la
                where la.tenantId = :tenantId
                and la.fingerprint = :fingerprint
                and la.reason='DEVICE_PENDING_APPROVAL'
                order by la.timestamp desc
            """)
    List<LoginAudit> findPendingAttempts(
            UUID tenantId,
            String fingerprint
    );
}