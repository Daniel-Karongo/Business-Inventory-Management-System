package com.IntegrityTechnologies.business_manager.security.biometric.dto;

import lombok.Data;

import java.util.UUID;

@Data
public class WebAuthnVerifyRequest {

    private String rawJson;
//    private String credentialId;
//    private String clientDataJSON;
//    private String authenticatorData;
//    private String signature;

    private UUID branchId;
    private String deviceId;
    private Double latitude;
    private Double longitude;
    private Double accuracy;
}