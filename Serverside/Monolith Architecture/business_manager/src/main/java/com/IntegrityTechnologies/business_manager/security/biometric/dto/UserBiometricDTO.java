package com.IntegrityTechnologies.business_manager.security.biometric.dto;

import lombok.Builder;
import lombok.Data;

import java.time.LocalDateTime;
import java.util.UUID;

@Data
@Builder
public class UserBiometricDTO {

    private UUID id;

    private String deviceName;
    private String deviceId;

    private String browserName;
    private String osName;
    private String platform;

    private LocalDateTime firstSeenAt;
    private LocalDateTime lastSeenAt;

    private String trustedDeviceStatus;

}