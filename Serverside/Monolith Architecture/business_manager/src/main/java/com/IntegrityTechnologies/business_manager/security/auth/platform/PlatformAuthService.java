package com.IntegrityTechnologies.business_manager.security.auth.platform;

import com.IntegrityTechnologies.business_manager.exception.AppSecurityException;
import com.IntegrityTechnologies.business_manager.modules.platform.identity.entity.PlatformUser;
import com.IntegrityTechnologies.business_manager.modules.platform.identity.repository.PlatformUserRepository;
import com.IntegrityTechnologies.business_manager.modules.platform.identity.repository.PlatformUserSessionRepository;
import com.IntegrityTechnologies.business_manager.security.auth.common.AuthResponseFactory;
import com.IntegrityTechnologies.business_manager.security.auth.common.JwtFactory;
import com.IntegrityTechnologies.business_manager.security.auth.dto.AuthRequest;
import com.IntegrityTechnologies.business_manager.security.auth.dto.AuthResponse;
import com.IntegrityTechnologies.business_manager.security.device.model.TrustedDevice;
import com.IntegrityTechnologies.business_manager.security.device.service.DeviceSecurityService;
import com.IntegrityTechnologies.business_manager.security.device.service.DeviceUsageService;
import com.IntegrityTechnologies.business_manager.security.model.SecurityErrorCode;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import jakarta.servlet.http.HttpServletRequest;
import lombok.RequiredArgsConstructor;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;

import java.util.UUID;

@Service
@RequiredArgsConstructor
public class PlatformAuthService {

    private static final int PLATFORM_USERS_MAX_SESSIONS_PER_DAY = 5;

    private final PlatformUserRepository platformUserRepository;
    private final PlatformUserSessionRepository platformUserSessionRepository;
    private final DeviceSecurityService deviceSecurityService;
    private final DeviceUsageService deviceUsageService;
    private final PasswordEncoder passwordEncoder;
    private final JwtFactory jwtFactory;
    private final AuthResponseFactory responseFactory;
    private final PlatformSessionService sessionService;

    private UUID tokenId() {
        return UUID.randomUUID();
    }
    public record Result(String jwt, AuthResponse response) {}

    public Result login(AuthRequest request, HttpServletRequest httpRequest) {

        String identifier = request.getIdentifier();

        PlatformUser platformUser = platformUserRepository
                .findByIdentifier(identifier)
                .orElse(null);

        if (platformUser == null) {
            return null; // allow fallback to tenant auth
        }

        if (request.getDeviceId() == null || request.getDeviceId().isBlank()) {
            throw new AppSecurityException(SecurityErrorCode.DEVICE_ID_REQUIRED, "Device ID required");
        }

        UUID platformTenantId = TenantContext.getTenantId();

        String ip = extractClientIp(httpRequest);

        TrustedDevice device = deviceSecurityService.validate(
            platformTenantId,
            null,
            request.getDeviceId(),
            platformUser.getId(),
            request,
            ip
        );

        if (!passwordEncoder.matches(request.getPassword(), platformUser.getPassword())) {
            throw new AppSecurityException(SecurityErrorCode.INVALID_CREDENTIALS, "Invalid credentials");
        }

        if (Boolean.TRUE.equals(platformUser.getMustChangePassword())) {
            throw new AppSecurityException(
                    SecurityErrorCode.PASSWORD_CHANGE_REQUIRED,
                    "Password change required"
            );
        }

        if (!platformUser.isActive() || platformUser.isLocked()) {
            throw new AppSecurityException(SecurityErrorCode.ACCOUNT_DISABLED, "Account disabled");
        }

        UUID tokenId = tokenId();

        String token = jwtFactory.generatePlatformToken(
                platformUser.getId(),
                platformUser.getUsername(),
                platformUser.getRole().name(),
                tokenId,
                request.getDeviceId()
        );

        long activeSessions =
                platformUserSessionRepository
                        .countByUserIdAndLogoutTimeIsNull(platformUser.getId());

        if (activeSessions >= PLATFORM_USERS_MAX_SESSIONS_PER_DAY) {
            throw new AppSecurityException(SecurityErrorCode.DEVICE_LIMIT_REACHED, "Too many active sessions");
        }

        deviceUsageService.record(device.getId(), platformUser.getId());

        sessionService.create(platformUser.getId(), tokenId);

        AuthResponse response = responseFactory.platform(
                platformUser.getId(),
                platformUser.getUsername(),
                platformUser.getRole().name()
        );

        return new Result(token, response);
    }

    public Result biometricLogin(AuthRequest request, HttpServletRequest httpRequest) {

        UUID userId = request.getUserId();

        if (userId == null) {
            throw new AppSecurityException(
                    SecurityErrorCode.INVALID_REQUEST,
                    "Invalid biometric user"
            );
        }

        if (request.getDeviceId() == null || request.getDeviceId().isBlank()) {
            throw new AppSecurityException(SecurityErrorCode.DEVICE_ID_REQUIRED, "Device ID required");
        }

        PlatformUser platformUser = platformUserRepository
                .findById(userId)
                .orElseThrow(() -> new AppSecurityException(
                        SecurityErrorCode.USER_NOT_FOUND,
                        "Platform user not found"
                ));

        if (!platformUser.isActive() || platformUser.isLocked()) {
            throw new AppSecurityException(SecurityErrorCode.ACCOUNT_DISABLED, "Account disabled");
        }

        UUID platformTenantId = TenantContext.getTenantId();

        String ip = extractClientIp(httpRequest);

        TrustedDevice device = deviceSecurityService.validate(
            platformTenantId,
            null,
            request.getDeviceId(),
            platformUser.getId(),
            request,
            ip
        );

        UUID tokenId = tokenId();

        String token = jwtFactory.generatePlatformToken(
                platformUser.getId(),
                platformUser.getUsername(),
                platformUser.getRole().name(),
                tokenId,
                request.getDeviceId()
        );

        long activeSessions =
                platformUserSessionRepository
                        .countByUserIdAndLogoutTimeIsNull(platformUser.getId());

        if (activeSessions >= PLATFORM_USERS_MAX_SESSIONS_PER_DAY) {
            throw new AppSecurityException(SecurityErrorCode.DEVICE_LIMIT_REACHED, "Too many active sessions");
        }

        deviceUsageService.record(device.getId(), platformUser.getId());

        sessionService.create(platformUser.getId(), tokenId);

        AuthResponse response = responseFactory.platform(
                platformUser.getId(),
                platformUser.getUsername(),
                platformUser.getRole().name()
        );

        return new Result(token, response);
    }

    private String extractClientIp(HttpServletRequest request) {
        String xf = request.getHeader("X-Forwarded-For");
        if (xf != null && !xf.isBlank()) {
            return xf.split(",")[0].trim();
        }
        return request.getRemoteAddr();
    }
}