package com.IntegrityTechnologies.business_manager.security.biometric.dto;

import lombok.Data;

import java.util.Map;
import java.util.UUID;

@Data
public class WebAuthnVerifyRequest {

    // ✅ PURE WEBAUTHN PAYLOAD ONLY
    private Map<String, Object> credential;

    // ✅ METADATA
    private UUID branchId;
    private String deviceId;
    private Double latitude;
    private Double longitude;
    private Double accuracy;
}