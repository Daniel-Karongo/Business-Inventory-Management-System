package com.IntegrityTechnologies.business_manager.security.auth.dto;

import lombok.Data;

import java.util.UUID;

@Data
public class AuthRequest {

    private UUID branchId;
    private String identifier;
    private String password;
    private UUID userId;

    private Double latitude;
    private Double longitude;
    private Double accuracy;

    private String deviceId;
}