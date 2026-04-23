package com.IntegrityTechnologies.business_manager.security.biometric.dto;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class BiometricStatsDTO {

    private long activeCredentials;

    private long uniqueUsers;

    private long uniqueDevices;
}