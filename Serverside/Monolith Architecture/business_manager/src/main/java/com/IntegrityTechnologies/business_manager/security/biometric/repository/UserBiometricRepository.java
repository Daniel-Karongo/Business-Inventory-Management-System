package com.IntegrityTechnologies.business_manager.security.biometric.repository;

import com.IntegrityTechnologies.business_manager.security.biometric.model.UserBiometric;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface UserBiometricRepository extends JpaRepository<UserBiometric, UUID> {

    Optional<UserBiometric> findByCredentialId(String credentialId);

    List<UserBiometric> findByTenantIdAndUserIdAndDeletedFalse(UUID tenantId, UUID userId);

    Optional<UserBiometric> findByIdAndTenantId(UUID id, UUID tenantId);

    long countByTenantIdAndUserIdAndDeletedFalse(UUID tenantId, UUID userId);
}