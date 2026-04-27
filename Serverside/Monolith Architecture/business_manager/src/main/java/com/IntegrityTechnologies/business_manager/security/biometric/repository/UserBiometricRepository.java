package com.IntegrityTechnologies.business_manager.security.biometric.repository;

import com.IntegrityTechnologies.business_manager.security.biometric.model.UserBiometric;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface UserBiometricRepository extends JpaRepository<UserBiometric, UUID> {

    Optional<UserBiometric> findByCredentialId(String credentialId);
    Optional<UserBiometric> findByTenantIdAndCredentialId(UUID tenantId, String credentialId);

    List<UserBiometric> findByTenantIdAndUserIdAndDeletedFalse(UUID tenantId, UUID userId);
    List<UserBiometric> findByTenantIdAndDeviceIdAndDeletedFalse(UUID tenantId, String deviceId);

    Optional<UserBiometric> findByIdAndTenantId(UUID id, UUID tenantId);
    List<UserBiometric> findByTenantIdAndDeletedFalse(UUID tenantId);
    Optional<UserBiometric> findByTenantIdAndUserIdAndDeviceIdAndDeletedFalse(
            UUID tenantId,
            UUID userId,
            String deviceId
    );
    Optional<UserBiometric> findByTenantIdAndUserIdAndDeviceId(
            UUID tenantId,
            UUID userId,
            String deviceId
    );
}