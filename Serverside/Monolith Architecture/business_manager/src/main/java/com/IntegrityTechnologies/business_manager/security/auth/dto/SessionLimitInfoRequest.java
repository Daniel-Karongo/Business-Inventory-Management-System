package com.IntegrityTechnologies.business_manager.security.auth.dto;

import lombok.Data;

import java.util.UUID;

@Data
public class SessionLimitInfoRequest {

    private String identifier;

    private UUID branchId;

    private String deviceId;
}