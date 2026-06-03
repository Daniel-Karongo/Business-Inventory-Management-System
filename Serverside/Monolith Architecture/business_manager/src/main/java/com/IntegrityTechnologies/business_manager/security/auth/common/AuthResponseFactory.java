package com.IntegrityTechnologies.business_manager.security.auth.common;

import com.IntegrityTechnologies.business_manager.security.auth.dto.AuthResponse;
import com.IntegrityTechnologies.business_manager.security.auth.model.UserType;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.UUID;

@Component
public class AuthResponseFactory {

    public AuthResponse platform(
            UUID userId,
            String username,
            String role
    ) {
        return new AuthResponse(
                userId,
                username,
                role,
                null,
                UserType.PLATFORM
        );
    }

    public AuthResponse tenant(
            UUID userId,
            String username,
            String role,
            UUID branchId
    ) {
        return new AuthResponse(
                userId,
                username,
                role,
                branchId,
                UserType.TENANT
        );
    }
}