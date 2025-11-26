package com.IntegrityTechnologies.business_manager.modules.biometric.repository;

import com.IntegrityTechnologies.business_manager.modules.biometric.model.BiometricRecord;
import com.IntegrityTechnologies.business_manager.modules.biometric.model.BiometricType;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

@Repository
public interface BiometricRecordRepository extends JpaRepository<BiometricRecord, UUID> {
    List<BiometricRecord> findByUserIdAndTypeAndDeletedFalse(UUID userId, BiometricType type);
    Optional<BiometricRecord> findByProviderId(String providerId);
    List<BiometricRecord> findByUserIdAndDeletedFalse(UUID userId);
    Optional<BiometricRecord> findByTemplateHash(String templateHash);
}
