package com.IntegrityTechnologies.business_manager.security.device.model;

import lombok.Builder;
import lombok.Data;

import java.util.UUID;

@Builder
@Data
public class DeviceRegistrationContext {

    private UUID tenantId;
    private UUID branchId;

    private String deviceId;

    private UUID attemptingUserId;

    private String deviceName;

    private String browserName;
    private String osName;
    private String platform;
    private String userAgent;

    private String ipAddress;
}