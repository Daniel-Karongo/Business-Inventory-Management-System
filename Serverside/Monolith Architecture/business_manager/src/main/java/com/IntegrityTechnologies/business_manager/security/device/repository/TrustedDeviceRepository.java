package com.IntegrityTechnologies.business_manager.security.device.repository;

import com.IntegrityTechnologies.business_manager.security.device.model.TrustedDevice;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.Optional;
import java.util.UUID;

@Repository
public interface TrustedDeviceRepository extends JpaRepository<TrustedDevice, UUID> {

    Optional<TrustedDevice> findByTenantIdAndBranchIdAndFingerprint(
            UUID tenantId,
            UUID branchId,
            String fingerprint
    );
}