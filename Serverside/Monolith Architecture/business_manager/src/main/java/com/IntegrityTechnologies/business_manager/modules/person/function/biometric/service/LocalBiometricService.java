package com.IntegrityTechnologies.business_manager.modules.person.function.biometric.service;

import com.IntegrityTechnologies.business_manager.exception.BiometricException;
import com.IntegrityTechnologies.business_manager.modules.person.function.biometric.model.BiometricRecord;
import com.IntegrityTechnologies.business_manager.modules.person.function.biometric.model.BiometricType;
import com.IntegrityTechnologies.business_manager.modules.person.function.biometric.repository.BiometricRecordRepository;
import lombok.RequiredArgsConstructor;
import org.apache.commons.codec.digest.DigestUtils;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.Base64;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class LocalBiometricService implements BiometricService {

    private final BiometricRecordRepository biometricRecordRepository;

    // Use a secure one-way hash function to store template identifiers.
    private String hashTemplate(byte[] template) {
        // SHA-256 via apache commons codec, then base64 encode
        byte[] sha = DigestUtils.sha256(template);
        return Base64.getEncoder().encodeToString(sha);
    }

    @Override
    public BiometricRecord enroll(UUID userId, BiometricType type, byte[] rawTemplate, String provider) throws BiometricException {
        try {
            String templateHash = hashTemplate(rawTemplate);
            BiometricRecord r = BiometricRecord.builder()
                    .user(null) // set association in service after load if needed
                    .type(type)
                    .providerId(provider)
                    .templateHash(templateHash)
                    .enrolledAt(LocalDateTime.now())
                    .deleted(false)
                    .build();
            return biometricRecordRepository.save(r);
        } catch (Exception e) {
            throw new BiometricException("Failed to enroll biometric", e);
        }
    }

    @Override
    public Optional<BiometricRecord> verify(UUID userId, BiometricType type, byte[] rawTemplate) throws BiometricException {
        try {
            String hash = hashTemplate(rawTemplate);
            List<BiometricRecord> recs = biometricRecordRepository.findByUserIdAndTypeAndDeletedFalse(userId, type);
            return recs.stream().filter(r -> Objects.equals(r.getTemplateHash(), hash)).findFirst();
        } catch (Exception e) {
            throw new BiometricException("Failed to verify biometric", e);
        }
    }

    @Override
    public Optional<BiometricRecord> identify(BiometricType type, byte[] rawTemplate) throws BiometricException {
        try {
            String hash = hashTemplate(rawTemplate);
            return biometricRecordRepository.findAll().stream()
                    .filter(r -> !r.isDeleted() && r.getType() == type && Objects.equals(r.getTemplateHash(), hash))
                    .findFirst();
        } catch (Exception e) {
            throw new BiometricException("Failed to identify biometric", e);
        }
    }

    // LocalBiometricService
    @Override
    public void deleteRecord(UUID id) {
        biometricRecordRepository.findById(id).ifPresent(r -> {
            r.setDeleted(true);
            biometricRecordRepository.save(r);
        });
    }
}