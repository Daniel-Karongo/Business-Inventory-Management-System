package com.IntegrityTechnologies.business_manager.security.auth.service;

import com.IntegrityTechnologies.business_manager.exception.UserNotFoundException;
import com.IntegrityTechnologies.business_manager.modules.person.branch.repository.BranchRepository;
import com.IntegrityTechnologies.business_manager.modules.person.department.model.Department;
import com.IntegrityTechnologies.business_manager.modules.person.department.repository.DepartmentRepository;
import com.IntegrityTechnologies.business_manager.modules.person.system.rollcall.model.RollcallMethod;
import com.IntegrityTechnologies.business_manager.modules.person.system.rollcall.model.UserSession;
import com.IntegrityTechnologies.business_manager.modules.person.system.rollcall.repository.UserSessionRepository;
import com.IntegrityTechnologies.business_manager.modules.person.system.rollcall.service.RollcallService;
import com.IntegrityTechnologies.business_manager.modules.person.user.model.User;
import com.IntegrityTechnologies.business_manager.modules.person.user.repository.UserRepository;
import com.IntegrityTechnologies.business_manager.modules.platform.identity.entity.PlatformUser;
import com.IntegrityTechnologies.business_manager.modules.platform.identity.entity.PlatformUserSession;
import com.IntegrityTechnologies.business_manager.modules.platform.identity.repository.PlatformUserRepository;
import com.IntegrityTechnologies.business_manager.modules.platform.identity.repository.PlatformUserSessionRepository;
import com.IntegrityTechnologies.business_manager.security.audit.service.LoginAuditService;
import com.IntegrityTechnologies.business_manager.security.auth.dto.AuthRequest;
import com.IntegrityTechnologies.business_manager.security.auth.dto.AuthResponse;
import com.IntegrityTechnologies.business_manager.security.auth.model.UserType;
import com.IntegrityTechnologies.business_manager.security.auth.platform.PlatformAuthService;
import com.IntegrityTechnologies.business_manager.security.auth.tenant.TenantAuthService;
import com.IntegrityTechnologies.business_manager.security.auth.util.JwtUtil;
import com.IntegrityTechnologies.business_manager.security.device.model.TrustedDevice;
import com.IntegrityTechnologies.business_manager.security.device.service.DeviceSecurityService;
import com.IntegrityTechnologies.business_manager.security.device.service.DeviceUsageService;
import com.IntegrityTechnologies.business_manager.security.device.service.LocationSecurityService;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import jakarta.servlet.http.HttpServletRequest;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.server.ResponseStatusException;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class AuthService {

    private static final int TENANT_USERS_MAX_SESSIONS_PER_DAY = 5;
    private static final int PLATFORM_USERS_MAX_SESSIONS_PER_DAY = 5;

    private final JwtUtil jwtUtil;
    private final UserRepository userRepository;
    private final PasswordEncoder passwordEncoder;
    private final RollcallService rollcallService;
    private final DepartmentRepository departmentRepository;
    private final BranchRepository branchRepository;
    private final UserSessionRepository userSessionRepository;
    private final TokenBlacklistService tokenBlacklistService;
    private final PlatformUserRepository platformUserRepository;
    private final PlatformUserSessionRepository platformUserSessionRepository;
    private final LoginAuditService loginAuditService;
    private final DeviceSecurityService deviceSecurityService;
    private final LocationSecurityService locationSecurityService;
    private final DeviceUsageService deviceUsageService;
    private final PlatformAuthService platformAuthService;
    private final TenantAuthService tenantAuthService;

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

        UserSession session =
                userSessionRepository
                        .findByTokenIdAndLogoutTimeIsNull(tokenId)
                        .orElse(null);

        if (session == null) return;

        session.logout(LocalDateTime.now(), auto);
        userSessionRepository.save(session);

        UUID userId = session.getUserId();
        UUID branchId = session.getBranchId();

        List<Department> departments =
                departmentRepository.findDepartmentsForUserInBranch(
                        tenantId(),
                        userId,
                        branchId
                );

        if (departments.isEmpty()) {
            rollcallService.recordLogoutRollcall(userId, null, branchId);
        } else {
            for (Department d : departments) {
                rollcallService.recordLogoutRollcall(userId, d.getId(), branchId);
            }
        }
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

        List<UserSession> sessions =
                userSessionRepository.findAllByUserIdAndLogoutTimeIsNull(userId);

        LocalDateTime now = LocalDateTime.now();

        for (UserSession s : sessions) {
            tokenBlacklistService.blacklistToken(s.getTokenId().toString());
            s.logout(now, auto);
            userSessionRepository.save(s);

            UUID branchId = s.getBranchId();
            List<Department> departments =
                    departmentRepository.findDepartmentsForUserInBranch(
                            tenantId(),
                            userId,
                            branchId
                    );

            if (departments.isEmpty()) {
                rollcallService.recordLogoutRollcall(userId, null, branchId);
            } else {
                for (Department d : departments) {
                    rollcallService.recordLogoutRollcall(userId, d.getId(), branchId);
                }
            }
        }
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
                    .orElseThrow(() -> new RuntimeException("Platform user not found"));

            return new AuthResponse(
                    user.getId(),
                    user.getUsername(),
                    user.getRole().name(),
                    null,
                    List.of(),
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
                .orElseThrow(() -> new RuntimeException("Session expired"));

        User user = userRepository.findById(userId)
                .orElseThrow(() -> new UserNotFoundException("User not found"));

        UserSession session =
                userSessionRepository.findByTokenIdAndLogoutTimeIsNull(tokenId)
                        .orElseThrow();

        List<Department> departments =
                departmentRepository.findDepartmentsForUserInBranch(
                        tenantId(),
                        userId,
                        session.getBranchId()
                );

        return new AuthResponse(
                userId,
                user.getUsername(),
                user.getRole().name(),
                session.getBranchId(),
                departments.stream().map(Department::getId).toList(),
                UserType.TENANT
        );
    }
}