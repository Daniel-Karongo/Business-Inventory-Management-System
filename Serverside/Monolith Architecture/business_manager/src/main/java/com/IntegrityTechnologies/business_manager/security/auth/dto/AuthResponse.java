package com.IntegrityTechnologies.business_manager.security.auth.dto;

import com.IntegrityTechnologies.business_manager.security.auth.model.UserType;
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

    private String username;

    private String role;

    private UUID branchId;

    private List<UUID> departmentIds;

    private UserType userType;

}