package com.IntegrityTechnologies.business_manager.security.biometric.dto;

import lombok.AllArgsConstructor;
import lombok.Data;

@Data
@AllArgsConstructor
public class WebAuthnChallengeResponse {
    private String challenge;
}