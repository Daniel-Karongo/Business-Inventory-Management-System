package com.IntegrityTechnologies.business_manager.security.biometric.repository;

import com.IntegrityTechnologies.business_manager.security.biometric.model.UserBiometric;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;
import java.util.UUID;

public interface UserBiometricRepository extends JpaRepository<UserBiometric, UUID> {

    Optional<UserBiometric> findByCredentialId(String credentialId);
}