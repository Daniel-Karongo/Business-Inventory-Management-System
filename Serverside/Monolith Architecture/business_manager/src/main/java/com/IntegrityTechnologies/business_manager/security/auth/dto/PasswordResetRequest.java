package com.IntegrityTechnologies.business_manager.security.auth.dto;

import lombok.Data;

import java.util.UUID;

@Data
public class PasswordResetRequest {

    private UUID branchId;
    private String identifier;
    private String channel;
}