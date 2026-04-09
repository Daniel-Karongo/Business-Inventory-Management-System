package com.IntegrityTechnologies.business_manager.security.auth.platform;

import com.IntegrityTechnologies.business_manager.modules.platform.identity.entity.PlatformUser;
import com.IntegrityTechnologies.business_manager.modules.platform.identity.entity.PlatformUserSession;
import com.IntegrityTechnologies.business_manager.modules.platform.identity.repository.PlatformUserRepository;
import com.IntegrityTechnologies.business_manager.modules.platform.identity.repository.PlatformUserSessionRepository;
import com.IntegrityTechnologies.business_manager.security.auth.common.AuthResponseFactory;
import com.IntegrityTechnologies.business_manager.security.auth.common.JwtFactory;
import com.IntegrityTechnologies.business_manager.security.auth.dto.AuthRequest;
import com.IntegrityTechnologies.business_manager.security.auth.dto.AuthResponse;
import com.IntegrityTechnologies.business_manager.security.device.model.TrustedDevice;
import com.IntegrityTechnologies.business_manager.security.device.service.DeviceSecurityService;
import com.IntegrityTechnologies.business_manager.security.device.service.DeviceUsageService;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import jakarta.servlet.http.HttpServletRequest;
import lombok.RequiredArgsConstructor;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;

import java.time.LocalDate;
import java.time.LocalDateTime;
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
                .findByUsernameAndDeletedFalse(identifier)
                .orElse(null);

        if (platformUser == null) {
            return null; // IMPORTANT: fallback to tenant flow
        }

        if (request.getDeviceId() == null || request.getDeviceId().isBlank()) {
            throw new BadCredentialsException("Device ID required");
        }

        UUID platformTenantId = TenantContext.getTenantId();

        deviceSecurityService.validate(platformTenantId, null, request.getDeviceId());

        if (!passwordEncoder.matches(request.getPassword(), platformUser.getPassword())) {
            throw new BadCredentialsException("Invalid credentials");
        }

        if (!platformUser.isActive() || platformUser.isLocked()) {
            throw new BadCredentialsException("Account disabled");
        }

        if (Boolean.TRUE.equals(platformUser.getMustChangePassword())) {
            throw new RuntimeException("PASSWORD_CHANGE_REQUIRED");
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
            throw new IllegalStateException("Too many active sessions");
        }

        TrustedDevice device = deviceSecurityService
                .getByDeviceId(platformTenantId, null, request.getDeviceId());

        deviceUsageService.record(device.getId(), platformUser.getId());

        PlatformUserSession session = PlatformUserSession.builder()
                .userId(platformUser.getId())
                .tokenId(tokenId)
                .loginDate(LocalDate.now())
                .loginTime(LocalDateTime.now())
                .autoLoggedOut(false)
                .build();

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
            throw new BadCredentialsException("Invalid biometric user");
        }

        if (request.getDeviceId() == null || request.getDeviceId().isBlank()) {
            throw new BadCredentialsException("Device ID required");
        }

        PlatformUser user = platformUserRepository
                .findById(userId)
                .orElseThrow(() -> new BadCredentialsException("Platform user not found"));

        if (!user.isActive() || user.isLocked()) {
            throw new BadCredentialsException("Account disabled");
        }

        UUID tokenId = tokenId();

        String token = jwtFactory.generatePlatformToken(
                user.getId(),
                user.getUsername(),
                user.getRole().name(),
                tokenId,
                request.getDeviceId()
        );

        long activeSessions =
                platformUserSessionRepository
                        .countByUserIdAndLogoutTimeIsNull(user.getId());

        if (activeSessions >= PLATFORM_USERS_MAX_SESSIONS_PER_DAY) {
            throw new IllegalStateException("Too many active sessions");
        }

        UUID platformTenantId = TenantContext.getTenantId();

        TrustedDevice device = deviceSecurityService
                .getByDeviceId(platformTenantId, null, request.getDeviceId());

        deviceUsageService.record(device.getId(), user.getId());

        sessionService.create(user.getId(), tokenId);

        AuthResponse response = responseFactory.platform(
                user.getId(),
                user.getUsername(),
                user.getRole().name()
        );

        return new Result(token, response);
    }
}