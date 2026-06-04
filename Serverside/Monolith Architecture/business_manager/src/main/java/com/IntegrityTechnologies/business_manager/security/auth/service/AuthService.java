package com.IntegrityTechnologies.business_manager.security.auth.service;

import com.IntegrityTechnologies.business_manager.exception.AppSecurityException;
import com.IntegrityTechnologies.business_manager.modules.person.branch.repository.BranchRepository;
import com.IntegrityTechnologies.business_manager.modules.person.system.rollcall.model.UserSession;
import com.IntegrityTechnologies.business_manager.modules.person.system.rollcall.repository.UserSessionRepository;
import com.IntegrityTechnologies.business_manager.modules.person.system.rollcall.service.RollcallService;
import com.IntegrityTechnologies.business_manager.modules.person.user.model.User;
import com.IntegrityTechnologies.business_manager.modules.person.user.repository.UserRepository;
import com.IntegrityTechnologies.business_manager.modules.platform.identity.entity.PlatformUser;
import com.IntegrityTechnologies.business_manager.modules.platform.identity.repository.PlatformUserRepository;
import com.IntegrityTechnologies.business_manager.modules.platform.identity.repository.PlatformUserSessionRepository;
import com.IntegrityTechnologies.business_manager.security.auth.dto.AuthRequest;
import com.IntegrityTechnologies.business_manager.security.auth.dto.AuthResponse;
import com.IntegrityTechnologies.business_manager.security.auth.dto.SessionLimitInfoResponse;
import com.IntegrityTechnologies.business_manager.security.auth.dto.UserSessionDTO;
import com.IntegrityTechnologies.business_manager.security.auth.model.UserType;
import com.IntegrityTechnologies.business_manager.security.auth.platform.PlatformAuthService;
import com.IntegrityTechnologies.business_manager.security.auth.tenant.TenantAuthService;
import com.IntegrityTechnologies.business_manager.security.auth.util.JwtUtil;
import com.IntegrityTechnologies.business_manager.security.model.SecurityErrorCode;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import jakarta.servlet.http.HttpServletRequest;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class AuthService {

    private final JwtUtil jwtUtil;
    private final UserRepository userRepository;
    private final RollcallService rollcallService;
    private final UserSessionRepository userSessionRepository;
    private final TokenBlacklistService tokenBlacklistService;
    private final PlatformUserRepository platformUserRepository;
    private final PlatformUserSessionRepository platformUserSessionRepository;
    private final PlatformAuthService platformAuthService;
    private final TenantAuthService tenantAuthService;
    private final BranchRepository branchRepository;

    private UUID tenantId() {
        return TenantContext.getTenantId();
    }
    /* =====================================================
       INTERNAL LOGIN RESULT (shared by login & bulk-login)
       ===================================================== */
    public record LoginResult(String jwt, AuthResponse response) {}

    /* =====================================================
       LOGIN CORE LOGIC (UNCHANGED, JUST EXTRACTED)
       ===================================================== */
    @Transactional
    public LoginResult loginInternal(AuthRequest request, HttpServletRequest httpRequest) {

        String identifier = request.getIdentifier();

    /* =====================================================
       1️⃣ TRY PLATFORM LOGIN
    ===================================================== */

        var platformResult = platformAuthService.login(request, httpRequest);

        if (platformResult != null) {
            return new LoginResult(
                    platformResult.jwt(),
                    platformResult.response()
            );
        }

    /* =====================================================
       2️⃣ TENANT LOGIN
    ===================================================== */

        var tenantResult = tenantAuthService.login(request, httpRequest);

        return new LoginResult(
                tenantResult.jwt(),
                tenantResult.response()
        );
    }

    private String extractClientIp(HttpServletRequest request) {

        String xf = request.getHeader("X-Forwarded-For");

        if (xf != null && !xf.isBlank()) {
            return xf.split(",")[0].trim();
        }

        return request.getRemoteAddr();
    }

    @Transactional
    public LoginResult loginInternalBiometric(AuthRequest request, HttpServletRequest httpRequest) {

        var result = tenantAuthService.biometricLogin(request, httpRequest);

        return new LoginResult(
                result.jwt(),
                result.response()
        );
    }

    /* =====================================================
       LOGOUT (single session)
       ===================================================== */
    @Transactional
    public void logout(UUID tokenId, boolean auto) {

        UserSession session = userSessionRepository
                .findByTokenIdAndLogoutTimeIsNull(tokenId)
                .orElseThrow(() -> new AppSecurityException(
                        SecurityErrorCode.SESSION_EXPIRED,
                        "Session expired"
                ));

        if (session == null) return;

        session.logout(LocalDateTime.now(), auto);
        userSessionRepository.save(session);

        UUID userId = session.getUserId();
        UUID branchId = session.getBranchId();

        rollcallService.recordLogoutRollcall(userId, branchId);

    }

    /* =====================================================
       LOGOUT BY TOKEN (COOKIE-BASED)
       ===================================================== */
    public void logoutByToken(String token) {

        tokenBlacklistService.blacklistToken(token);

        String userType = jwtUtil.extractUserType(token);

        if ("PLATFORM".equals(userType)) {

            UUID tokenId = jwtUtil.extractTokenId(token);

            platformUserSessionRepository
                    .findByTokenIdAndLogoutTimeIsNull(tokenId)
                    .ifPresent(session -> {
                        session.logout(LocalDateTime.now(), false);
                        platformUserSessionRepository.save(session);
                    });

            return;
        }

        UUID tokenId = jwtUtil.extractTokenId(token);

        logout(tokenId, false);
    }

    /* =====================================================
       LOGOUT ALL SESSIONS
       ===================================================== */
    @Transactional
    public void logoutAllSessions(UUID userId, boolean auto) {

        LocalDateTime now = LocalDateTime.now();

        platformUserSessionRepository
                .findAllByUserIdAndLogoutTimeIsNull(userId)
                .forEach(s -> {
                    s.logout(now, auto);
                    platformUserSessionRepository.save(s);
                });

        List<UserSession> sessions =
                userSessionRepository
                        .findAllByUserIdAndLogoutTimeIsNull(userId);

        for (UserSession s : sessions) {

            s.logout(now, auto);
            userSessionRepository.save(s);

            UUID branchId = s.getBranchId();

            rollcallService.recordLogoutRollcall(userId, branchId);

        }
    }

    @Transactional
    public void logoutSession(
            UUID userId,
            UUID tokenId
    ) {

        UserSession session =
                userSessionRepository
                        .findByUserIdAndTokenIdAndLogoutTimeIsNull(
                                userId,
                                tokenId
                        )
                        .orElseThrow(() ->
                                new AppSecurityException(
                                        SecurityErrorCode.SESSION_EXPIRED,
                                        "Session not found"
                                )
                        );

        session.logout(
                LocalDateTime.now(),
                false
        );

        userSessionRepository.save(session);

        rollcallService.recordLogoutRollcall(
                userId,
                session.getBranchId()
        );
    }

    @Transactional
    public void logoutAllExcept(
            UUID userId,
            UUID currentTokenId
    ) {

        LocalDateTime now = LocalDateTime.now();

        List<UserSession> sessions =
                userSessionRepository
                        .findAllByUserIdAndLogoutTimeIsNull(
                                userId
                        );

        for (UserSession session : sessions) {

            if (session.getTokenId().equals(currentTokenId)) {
                continue;
            }

            session.logout(now, false);

            userSessionRepository.save(session);

            rollcallService.recordLogoutRollcall(
                    userId,
                    session.getBranchId()
            );
        }
    }

    @Transactional(readOnly = true)
    public SessionLimitInfoResponse sessionLimitInfo(
            UUID tenantId,
            String identifier
    ) {

        User user =
                userRepository
                        .findAuthUser(
                                tenantId,
                                identifier
                        )
                        .orElseThrow(() ->
                                new AppSecurityException(
                                        SecurityErrorCode.USER_NOT_FOUND,
                                        "User not found"
                                )
                        );

        List<UserSession> activeSessions =
                userSessionRepository
                        .findAllByUserIdAndLogoutTimeIsNullOrderByLoginTimeDesc(
                                user.getId()
                        );

        Set<UUID> branchIds =
                activeSessions.stream()
                        .map(UserSession::getBranchId)
                        .filter(Objects::nonNull)
                        .collect(Collectors.toSet());

        Map<UUID,String> names =
                branchRepository
                        .findNames(branchIds)
                        .stream()
                        .collect(
                                Collectors.toMap(
                                        r -> (UUID) r[0],
                                        r -> (String) r[1]
                                )
                        );

        List<UserSessionDTO> sessions =
                activeSessions
                        .stream()
                        .map(s ->
                                new UserSessionDTO(

                                        s.getTokenId(),

                                        s.getBranchId(),

                                        names.getOrDefault(
                                                s.getBranchId(),
                                                "Unknown Branch"
                                        ),

                                        s.getLoginDate(),

                                        s.getLoginTime(),

                                        s.getDeviceId(),

                                        s.getDeviceName(),

                                        s.getBrowserName(),

                                        s.getOsName(),

                                        s.getPlatform(),

                                        s.getIpAddress(),

                                        false,

                                        false
                                )
                        )
                        .toList();

        return SessionLimitInfoResponse.builder()
                .limit(5)
                .activeSessions(
                        sessions.size()
                )
                .sessions(
                        sessions
                )
                .build();
    }

    @Transactional
    public void logoutSessionForRecovery(
            String identifier,
            String password,
            UUID tokenId,
            TenantAuthService tenantAuthService
    ) {

        User user =
                tenantAuthService
                        .validateCredentialsForRecovery(
                                identifier,
                                password
                        );

        UserSession session =
                userSessionRepository
                        .findByUserIdAndTokenIdAndLogoutTimeIsNull(
                                user.getId(),
                                tokenId
                        )
                        .orElseThrow(() ->
                                new AppSecurityException(
                                        SecurityErrorCode.SESSION_EXPIRED,
                                        "Session not found"
                                )
                        );

        session.logout(
                LocalDateTime.now(),
                false
        );

        userSessionRepository.save(
                session
        );

        rollcallService.recordLogoutRollcall(
                user.getId(),
                session.getBranchId()
        );
    }

    @Transactional
    public void logoutBySessionToken(
            UUID tokenId
    ) {

        UserSession session =
                userSessionRepository
                        .findByTokenIdAndLogoutTimeIsNull(
                                tokenId
                        )
                        .orElseThrow(() ->
                                new AppSecurityException(
                                        SecurityErrorCode.SESSION_EXPIRED,
                                        "Session not found"
                                )
                        );

        session.logout(
                LocalDateTime.now(),
                true
        );

        userSessionRepository.save(
                session
        );

        rollcallService.recordLogoutRollcall(
                session.getUserId(),
                session.getBranchId()
        );
    }

    /* =====================================================
       CURRENT USER (/me)
       ===================================================== */
    @Transactional(readOnly = true)
    public AuthResponse resolveMe(String token) {

        String userType = jwtUtil.extractUserType(token);

    /* ==========================================
       PLATFORM USER
    ========================================== */

        if ("PLATFORM".equals(userType)) {

            UUID userId = jwtUtil.extractUserId(token);

            PlatformUser user = platformUserRepository
                    .findById(userId)
                    .orElseThrow(() -> new AppSecurityException(
                            SecurityErrorCode.USER_NOT_FOUND,
                            "Platform user not found"
                    ));

            return new AuthResponse(
                    user.getId(),
                    user.getUsername(),
                    user.getRole().name(),
                    null,
                    UserType.PLATFORM
            );
        }

    /* ==========================================
       TENANT USER
    ========================================== */

        UUID userId = jwtUtil.extractUserId(token);
        UUID tokenId = jwtUtil.extractTokenId(token);

        userSessionRepository
                .findByTokenIdAndLogoutTimeIsNull(tokenId)
                .orElseThrow(() -> new AppSecurityException(
                        SecurityErrorCode.SESSION_EXPIRED,
                        "Session expired"
                ));

        User user = userRepository.findById(userId)
                .orElseThrow(() -> new AppSecurityException(
                        SecurityErrorCode.USER_NOT_FOUND,
                        "User not found"
                ));

        UserSession session =
                userSessionRepository.findByTokenIdAndLogoutTimeIsNull(tokenId)
                        .orElseThrow(() -> new AppSecurityException(
                                SecurityErrorCode.SESSION_EXPIRED,
                                "Session expired"
                        ));

        return new AuthResponse(
                userId,
                user.getUsername(),
                user.getRole().name(),
                session.getBranchId(),
                UserType.TENANT
        );
    }
}