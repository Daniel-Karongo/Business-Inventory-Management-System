package com.IntegrityTechnologies.business_manager.security.biometric.dto;

import lombok.Builder;
import lombok.Data;

import java.util.UUID;

@Data
@Builder
public class UserBiometricDTO {

    private UUID id;
    private String deviceName;
    private String fingerprint;
}