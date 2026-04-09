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
import com.IntegrityTechnologies.business_manager.security.auth.util.DeviceFingerprintUtil;
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

        var platformUserOpt =
                platformUserRepository.findByUsernameAndDeletedFalse(identifier);

        if (platformUserOpt.isPresent()) {

            PlatformUser platformUser = platformUserOpt.get();

            if (request.getDeviceId() == null || request.getDeviceId().isBlank()) {
                throw new BadCredentialsException("Device ID required");
            }

            String fingerprint =
                    DeviceFingerprintUtil.generate(httpRequest, request.getDeviceId());

            String ip = extractClientIp(httpRequest);

            /* ================= DEVICE VALIDATION ================= */
            UUID platformTenantId = tenantId();

            if (platformTenantId == null) {
                throw new IllegalStateException("Platform tenant not resolved");
            }

            deviceSecurityService.validate(platformTenantId, null, fingerprint);

            /* ================= PASSWORD AUTH ================= */
            if (!passwordEncoder.matches(request.getPassword(), platformUser.getPassword())) {
                throw new BadCredentialsException("Invalid credentials");
            }

            if (!platformUser.isActive() || platformUser.isLocked()) {
                throw new BadCredentialsException("Account disabled");
            }

            if (Boolean.TRUE.equals(platformUser.getMustChangePassword())) {
                throw new ResponseStatusException(
                        HttpStatus.PRECONDITION_REQUIRED,
                        "PASSWORD_CHANGE_REQUIRED"
                );
            }

            UUID tokenId = UUID.randomUUID();

            String token = jwtUtil.generateToken(
                    UserType.PLATFORM.name(),
                    null,
                    platformUser.getId(),
                    platformUser.getUsername(),
                    platformUser.getRole().name(),
                    tokenId,
                    null,
                    fingerprint
            );

            long activeSessions =
                    platformUserSessionRepository
                            .countByUserIdAndLogoutTimeIsNull(platformUser.getId());

            if (activeSessions >= PLATFORM_USERS_MAX_SESSIONS_PER_DAY) {
                throw new IllegalStateException("Too many active sessions");
            }

            // ================= DEVICE USAGE TRACKING =================

            TrustedDevice device = deviceSecurityService
                    .getByFingerprint(platformTenantId, null, fingerprint);

            deviceUsageService.record(device.getId(), platformUser.getId());

            PlatformUserSession session = PlatformUserSession.builder()
                    .userId(platformUser.getId())
                    .tokenId(tokenId)
                    .loginDate(LocalDate.now())
                    .loginTime(LocalDateTime.now())
                    .autoLoggedOut(false)
                    .build();

            platformUserSessionRepository.save(session);

            AuthResponse response = new AuthResponse(
                    platformUser.getId(),
                    platformUser.getUsername(),
                    platformUser.getRole().name(),
                    null,
                    List.of(),
                    UserType.PLATFORM
            );

            return new LoginResult(token, response);
        }

    /* =====================================================
       2️⃣ TENANT LOGIN
    ===================================================== */

        UUID tenantId = tenantId();
        UUID branchId = request.getBranchId();

        if (request.getDeviceId() == null || request.getDeviceId().isBlank()) {
            throw new BadCredentialsException("Device ID required");
        }

        String fingerprint =
                DeviceFingerprintUtil.generate(httpRequest, request.getDeviceId());

        String ip = extractClientIp(httpRequest);

        var branch = branchRepository
                .findByTenantIdAndIdAndDeletedFalse(tenantId, branchId)
                .orElseThrow(() -> new BadCredentialsException("Branch not found"));

        try {

            deviceSecurityService.validate(tenantId, branchId, fingerprint);

            locationSecurityService.validate(
                    branch,
                    request.getLatitude(),
                    request.getLongitude(),
                    request.getAccuracy()
            );

        } catch (Exception ex) {

            loginAuditService.log(
                    tenantId,
                    null,
                    branchId,
                    fingerprint,
                    request.getLatitude(),
                    request.getLongitude(),
                    request.getAccuracy(),
                    ip,
                    "BLOCKED",
                    ex.getMessage()
            );

            throw new BadCredentialsException(ex.getMessage());
        }

        /* ================= PASSWORD AUTH ================= */

        User user;

        identifier = request.getIdentifier();

        user = userRepository
                .findAuthUser(tenantId, identifier)
                .orElseThrow(() -> new UserNotFoundException("User not found"));

        if (!passwordEncoder.matches(request.getPassword(), user.getPassword())) {
            throw new BadCredentialsException("Invalid password");
        }

        /* ================= CONTINUE EXISTING FLOW ================= */

        UUID userId = user.getId();
        LocalDate today = LocalDate.now();

        boolean belongs = branchRepository.findBranchesByUserId(
                tenantId,
                userId
        ).stream().anyMatch(b -> b.getId().equals(branchId));

        if (!belongs) {
            throw new BadCredentialsException("User is not assigned to this branch");
        }

        int activeCount =
                userSessionRepository
                        .findByUserIdAndLoginDateAndLogoutTimeIsNull(userId, today)
                        .size();

        if (activeCount >= TENANT_USERS_MAX_SESSIONS_PER_DAY) {
            throw new IllegalStateException("Maximum active sessions reached for today");
        }

        UUID tokenId = UUID.randomUUID();

        String token = jwtUtil.generateToken(
                UserType.TENANT.name(),
                user.getTenantId(),
                userId,
                user.getUsername(),
                user.getRole().name(),
                tokenId,
                branchId,
                fingerprint
        );

        // ================= DEVICE USAGE TRACKING =================

        TrustedDevice device = deviceSecurityService
                .getByFingerprint(tenantId, branchId, fingerprint);

        deviceUsageService.record(device.getId(), userId);

        UserSession session = UserSession.builder()
                .tenantId(tenantId)
                .branchId(branchId)
                .userId(userId)
                .loginDate(today)
                .loginTime(LocalDateTime.now())
                .tokenId(tokenId)
                .autoLoggedOut(false)
                .build();

        userSessionRepository.save(session);

        List<Department> departments =
                departmentRepository.findDepartmentsForUserInBranch(
                        tenantId(),
                        userId,
                        branchId
                );

        if (departments.isEmpty()) {
            rollcallService.recordLoginRollcall(
                    userId,
                    null,
                    branchId,
                    RollcallMethod.LOGIN_PASSWORD
            );
        } else {
            for (Department d : departments) {
                rollcallService.recordLoginRollcall(
                        userId,
                        d.getId(),
                        branchId,
                        RollcallMethod.LOGIN_PASSWORD
                );
            }
        }

        AuthResponse response = new AuthResponse(
                userId,
                user.getUsername(),
                user.getRole().name(),
                branchId,
                departments.stream().map(Department::getId).toList(),
                UserType.TENANT
        );

        loginAuditService.log(
                tenantId,
                userId,
                branchId,
                fingerprint,
                request.getLatitude(),
                request.getLongitude(),
                request.getAccuracy(),
                ip,
                "SUCCESS",
                "OK"
        );

        return new LoginResult(token, response);
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

        UUID userId = request.getUserId();

        if (userId == null) {
            throw new BadCredentialsException("Invalid biometric user");
        }

        UUID tenantId = tenantId();
        UUID branchId = request.getBranchId();

        if (request.getDeviceId() == null || request.getDeviceId().isBlank()) {
            throw new BadCredentialsException("Device ID required");
        }

        String fingerprint =
                DeviceFingerprintUtil.generate(httpRequest, request.getDeviceId());

        String ip = extractClientIp(httpRequest);

        var branch = branchRepository
                .findByTenantIdAndIdAndDeletedFalse(tenantId, branchId)
                .orElseThrow(() -> new BadCredentialsException("Branch not found"));

        try {

            deviceSecurityService.validate(tenantId, branchId, fingerprint);

            locationSecurityService.validate(
                    branch,
                    request.getLatitude(),
                    request.getLongitude(),
                    request.getAccuracy()
            );

        } catch (Exception ex) {

            loginAuditService.log(
                    tenantId,
                    null,
                    branchId,
                    fingerprint,
                    request.getLatitude(),
                    request.getLongitude(),
                    request.getAccuracy(),
                    ip,
                    "BLOCKED",
                    ex.getMessage()
            );

            throw new BadCredentialsException(ex.getMessage());
        }

        User user = userRepository
                .findByIdAndTenantId(userId, tenantId)
                .orElseThrow(() -> new UserNotFoundException("User not found"));

        if (Boolean.TRUE.equals(user.getDeleted())) {
            throw new BadCredentialsException("Account deleted");
        }

        UUID resolvedUserId = user.getId();
        LocalDate today = LocalDate.now();

        boolean belongs = branchRepository.findBranchesByUserId(
                tenantId,
                resolvedUserId
        ).stream().anyMatch(b -> b.getId().equals(branchId));

        if (!belongs) {
            throw new BadCredentialsException("User is not assigned to this branch");
        }

        int activeCount =
                userSessionRepository
                        .findByUserIdAndLoginDateAndLogoutTimeIsNull(resolvedUserId, today)
                        .size();

        if (activeCount >= TENANT_USERS_MAX_SESSIONS_PER_DAY) {
            throw new IllegalStateException("Maximum active sessions reached for today");
        }

        UUID tokenId = UUID.randomUUID();

        String token = jwtUtil.generateToken(
                UserType.TENANT.name(),
                user.getTenantId(),
                resolvedUserId,
                user.getUsername(),
                user.getRole().name(),
                tokenId,
                branchId,
                fingerprint
        );

        // ================= DEVICE USAGE TRACKING =================

        TrustedDevice device = deviceSecurityService
                .getByFingerprint(tenantId, branchId, fingerprint);

        deviceUsageService.record(device.getId(), resolvedUserId);

        UserSession session = UserSession.builder()
                .tenantId(tenantId)
                .branchId(branchId)
                .userId(resolvedUserId)
                .loginDate(today)
                .loginTime(LocalDateTime.now())
                .tokenId(tokenId)
                .autoLoggedOut(false)
                .build();

        userSessionRepository.save(session);

        List<Department> departments =
                departmentRepository.findDepartmentsForUserInBranch(
                        tenantId(),
                        resolvedUserId,
                        branchId
                );

        if (departments.isEmpty()) {
            rollcallService.recordLoginRollcall(
                    resolvedUserId,
                    null,
                    branchId,
                    RollcallMethod.LOGIN_BIOMETRIC
            );
        } else {
            for (Department d : departments) {
                rollcallService.recordLoginRollcall(
                        resolvedUserId,
                        d.getId(),
                        branchId,
                        RollcallMethod.LOGIN_BIOMETRIC
                );
            }
        }

        AuthResponse response = new AuthResponse(
                resolvedUserId,
                user.getUsername(),
                user.getRole().name(),
                branchId,
                departments.stream().map(Department::getId).toList(),
                UserType.TENANT
        );

        loginAuditService.log(
                tenantId,
                resolvedUserId,
                branchId,
                fingerprint,
                request.getLatitude(),
                request.getLongitude(),
                request.getAccuracy(),
                ip,
                "SUCCESS",
                "OK"
        );

        return new LoginResult(token, response);
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