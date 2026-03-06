package com.IntegrityTechnologies.business_manager.security.auth.dto;

import lombok.AllArgsConstructor;
import lombok.Data;

import java.util.UUID;

@Data
@AllArgsConstructor
public class BulkAuthResponse {
    private UUID userId;
    private String username;
    private String role;
    private UUID branchId;
    private String token;
    private long expiresAt;
}