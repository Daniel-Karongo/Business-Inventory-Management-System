package com.IntegrityTechnologies.business_manager.security.auth.dto;

import lombok.Data;

import java.util.UUID;

@Data
public class SessionRecoveryRequest {

    private String identifier;

    private UUID branchId;

    private String password;

    private UUID tokenId;
}