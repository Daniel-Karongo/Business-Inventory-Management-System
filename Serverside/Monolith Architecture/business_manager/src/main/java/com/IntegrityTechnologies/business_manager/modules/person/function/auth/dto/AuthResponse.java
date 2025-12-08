package com.IntegrityTechnologies.business_manager.modules.person.function.auth.dto;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;
import java.util.UUID;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class AuthResponse {
    private UUID userId;
    private String userName;
    private String role;
    private UUID branchId;
    private List<UUID> departmentIds;
    private long expiresAt;
    private String token;
}