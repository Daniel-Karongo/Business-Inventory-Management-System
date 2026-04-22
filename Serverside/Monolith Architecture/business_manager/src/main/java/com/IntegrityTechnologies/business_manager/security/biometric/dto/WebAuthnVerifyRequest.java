package com.IntegrityTechnologies.business_manager.security.biometric.dto;

import lombok.Data;

import java.util.Map;
import java.util.UUID;

@Data
public class WebAuthnVerifyRequest {

    private Map<String,Object> credential;

    private UUID branchId;

    private String deviceId;

    private Double latitude;
    private Double longitude;
    private Double accuracy;

    // add
    private String browserName;
    private String osName;
    private String platform;
    private String userAgent;
}