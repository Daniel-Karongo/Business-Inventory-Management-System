package com.IntegrityTechnologies.business_manager.modules.biometric.service;

import com.IntegrityTechnologies.business_manager.exception.BiometricException;
import com.IntegrityTechnologies.business_manager.modules.biometric.model.BiometricRecord;
import com.IntegrityTechnologies.business_manager.modules.biometric.model.BiometricType;

import java.util.Optional;
import java.util.UUID;

public interface BiometricService {
    /**
     * Enroll template for a user. Returns BiometricRecord (with providerId / templateHash).
     */
    BiometricRecord enroll(UUID userId, BiometricType type, byte[] rawTemplate, String provider) throws BiometricException;

    /**
     * Verify given rawTemplate against user records (or external provider).
     * Returns matched BiometricRecord if match, otherwise empty.
     */
    Optional<BiometricRecord> verify(UUID userId, BiometricType type, byte[] rawTemplate) throws BiometricException;

    /**
     * Verify against ANY user (fingerprint kiosk that must find whose it is),
     * returns matched userId if found.
     */
    Optional<BiometricRecord> identify(BiometricType type, byte[] rawTemplate) throws BiometricException;

    void deleteRecord(UUID id);
}