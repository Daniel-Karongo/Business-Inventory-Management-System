package com.IntegrityTechnologies.business_manager.security.auth.service;

import com.IntegrityTechnologies.business_manager.exception.UserNotFoundException;
import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.repository.BranchRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.department.model.Department;
import com.IntegrityTechnologies.business_manager.modules.person.entity.department.repository.DepartmentRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.User;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.repository.UserRepository;
import com.IntegrityTechnologies.business_manager.modules.platform.identity.entity.PlatformUserSession;
import com.IntegrityTechnologies.business_manager.modules.platform.identity.repository.PlatformUserSessionRepository;
import com.IntegrityTechnologies.business_manager.security.auth.dto.AuthRequest;
import com.IntegrityTechnologies.business_manager.security.auth.dto.AuthResponse;
import com.IntegrityTechnologies.business_manager.security.auth.util.DeviceFingerprintUtil;
import com.IntegrityTechnologies.business_manager.security.auth.util.JwtUtil;
import com.IntegrityTechnologies.business_manager.modules.person.function.rollcall.model.UserSession;
import com.IntegrityTechnologies.business_manager.modules.person.function.rollcall.repository.UserSessionRepository;
import com.IntegrityTechnologies.business_manager.modules.person.function.rollcall.service.RollcallService;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.context.TenantContext;
import com.IntegrityTechnologies.business_manager.modules.platform.identity.entity.PlatformUser;
import com.IntegrityTechnologies.business_manager.modules.platform.identity.repository.PlatformUserRepository;
import com.IntegrityTechnologies.business_manager.security.auth.model.UserType;
import jakarta.servlet.http.HttpServletRequest;
import lombok.RequiredArgsConstructor;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class AuthService {

    private static final int MAX_SESSIONS_PER_DAY = 4;

    private final AuthenticationManager authenticationManager;
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

            if (!passwordEncoder.matches(request.getPassword(), platformUser.getPassword())) {
                throw new BadCredentialsException("Invalid credentials");
            }

            if (!platformUser.isActive() || platformUser.isLocked()) {
                throw new BadCredentialsException("Account disabled");
            }

            if (Boolean.TRUE.equals(platformUser.getMustChangePassword())) {
                throw new BadCredentialsException("PASSWORD_CHANGE_REQUIRED");
            }

            UUID tokenId = UUID.randomUUID();
            String deviceFingerprint = DeviceFingerprintUtil.generate(httpRequest);

            String token = jwtUtil.generateToken(
                    UserType.PLATFORM.name(),
                    null,
                    platformUser.getId(),
                    platformUser.getUsername(),
                    platformUser.getRole().name(),
                    tokenId,
                    null,
                    deviceFingerprint
            );

            long activeSessions =
                    platformUserSessionRepository
                            .countByUserIdAndLogoutTimeIsNull(platformUser.getId());

            if (activeSessions >= 1) {
                throw new IllegalStateException("Too many active sessions");
            }

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

        UUID tenantId = TenantContext.getTenantId();

        User user = userRepository
                .findAuthUser(tenantId, identifier)
                .orElseThrow(() -> new UserNotFoundException("User not found"));

        if (!passwordEncoder.matches(request.getPassword(), user.getPassword())) {
            throw new BadCredentialsException("Invalid password");
        }

        if (Boolean.TRUE.equals(user.getMustChangePassword())) {
            throw new BadCredentialsException("PASSWORD_CHANGE_REQUIRED");
        }

        authenticationManager.authenticate(
                new UsernamePasswordAuthenticationToken(
                        user.getUsername(),
                        request.getPassword()
                )
        );

        UUID userId = user.getId();
        UUID branchId = request.getBranchId();
        LocalDate today = LocalDate.now();

        branchRepository.findByIdAndDeletedFalse(branchId)
                .orElseThrow(() -> new BadCredentialsException("Selected branch does not exist"));

        boolean belongs = branchRepository.findBranchesByUserId(userId)
                .stream()
                .anyMatch(b -> b.getId().equals(branchId));

        if (!belongs) {
            throw new BadCredentialsException("User is not assigned to this branch");
        }

        int activeCount =
                userSessionRepository
                        .findByUserIdAndLoginDateAndLogoutTimeIsNull(userId, today)
                        .size();

        if (activeCount >= MAX_SESSIONS_PER_DAY) {
            throw new IllegalStateException("Maximum active sessions reached for today");
        }

        UUID tokenId = UUID.randomUUID();
        String deviceFingerprint = DeviceFingerprintUtil.generate(httpRequest);

        String token = jwtUtil.generateToken(
                UserType.TENANT.name(),
                user.getTenantId(),
                userId,
                user.getUsername(),
                user.getRole().name(),
                tokenId,
                branchId,
                deviceFingerprint
        );

        UserSession session = UserSession.builder()
                .userId(userId)
                .branchId(branchId)
                .loginDate(today)
                .loginTime(LocalDateTime.now())
                .tokenId(tokenId)
                .autoLoggedOut(false)
                .build();

        userSessionRepository.save(session);

        List<Department> departments =
                departmentRepository.findDepartmentsForUserInBranch(userId, branchId);

        if (departments.isEmpty()) {
            rollcallService.recordLoginRollcall(userId, null, branchId);
        } else {
            for (Department d : departments) {
                rollcallService.recordLoginRollcall(userId, d.getId(), branchId);
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
                departmentRepository.findDepartmentsForUserInBranch(userId, branchId);

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
                    departmentRepository.findDepartmentsForUserInBranch(userId, branchId);

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