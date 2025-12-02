package com.IntegrityTechnologies.business_manager.modules.person.function.auth.dto;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.UUID;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class AuthRequest {
    private UUID branchId;
    private String identifier;
    private String password;
}