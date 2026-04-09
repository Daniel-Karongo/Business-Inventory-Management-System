package com.IntegrityTechnologies.business_manager.security.auth.common;

import com.IntegrityTechnologies.business_manager.security.auth.model.UserType;
import com.IntegrityTechnologies.business_manager.security.auth.util.JwtUtil;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.UUID;

@Component
@RequiredArgsConstructor
public class JwtFactory {

    private final JwtUtil jwtUtil;

    public String generatePlatformToken(
            UUID userId,
            String username,
            String role,
            UUID tokenId,
            String deviceId
    ) {
        return jwtUtil.generateToken(
                UserType.PLATFORM.name(),
                null,
                userId,
                username,
                role,
                tokenId,
                null,
                deviceId
        );
    }

    public String generateTenantToken(
            UUID tenantId,
            UUID userId,
            String username,
            String role,
            UUID tokenId,
            UUID branchId,
            String deviceId
    ) {
        return jwtUtil.generateToken(
                UserType.TENANT.name(),
                tenantId,
                userId,
                username,
                role,
                tokenId,
                branchId,
                deviceId
        );
    }
}