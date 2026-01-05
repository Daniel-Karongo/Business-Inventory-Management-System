package com.IntegrityTechnologies.business_manager.modules.person.function.auth.service;

import com.IntegrityTechnologies.business_manager.exception.UserNotFoundException;
import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.repository.BranchRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.department.model.Department;
import com.IntegrityTechnologies.business_manager.modules.person.entity.department.repository.DepartmentRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.User;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.repository.UserRepository;
import com.IntegrityTechnologies.business_manager.modules.person.function.auth.dto.AuthRequest;
import com.IntegrityTechnologies.business_manager.modules.person.function.auth.dto.AuthResponse;
import com.IntegrityTechnologies.business_manager.modules.person.function.auth.util.JwtUtil;
import com.IntegrityTechnologies.business_manager.modules.person.function.rollcall.model.UserSession;
import com.IntegrityTechnologies.business_manager.modules.person.function.rollcall.repository.UserSessionRepository;
import com.IntegrityTechnologies.business_manager.modules.person.function.rollcall.service.RollcallService;
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

    /* =====================================================
       INTERNAL LOGIN RESULT (shared by login & bulk-login)
       ===================================================== */
    public record LoginResult(String jwt, AuthResponse response) {}

    /* =====================================================
       LOGIN CORE LOGIC (UNCHANGED, JUST EXTRACTED)
       ===================================================== */
    @Transactional
    public LoginResult loginInternal(AuthRequest request) {

        /* ---------- 1️⃣ Resolve user ---------- */
        User user = userRepository.findByIdentifier(request.getIdentifier())
                .orElseThrow(() -> new UserNotFoundException("User not found"));

        if (!passwordEncoder.matches(request.getPassword(), user.getPassword())) {
            throw new BadCredentialsException("Invalid password");
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

        /* ---------- 2️⃣ Validate branch ---------- */
        branchRepository.findByIdAndDeletedFalse(branchId)
                .orElseThrow(() -> new BadCredentialsException("Selected branch does not exist"));

        boolean belongs = branchRepository.findBranchesByUserId(userId)
                .stream()
                .anyMatch(b -> b.getId().equals(branchId));

        if (!belongs) {
            throw new BadCredentialsException("User is not assigned to this branch");
        }

        /* ---------- 3️⃣ Enforce daily session cap ---------- */
        int activeCount =
                userSessionRepository
                        .findByUserIdAndLoginDateAndLogoutTimeIsNull(userId, today)
                        .size();

        if (activeCount >= MAX_SESSIONS_PER_DAY) {
            throw new IllegalStateException("Maximum active sessions reached for today");
        }

        /* ---------- 4️⃣ Create JWT + session ---------- */
        UUID tokenId = UUID.randomUUID();

        String token = jwtUtil.generateToken(
                userId,
                user.getUsername(),
                user.getRole().name(),
                tokenId
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

        /* ---------- 5️⃣ Rollcall LOGIN ---------- */
        List<Department> departments =
                departmentRepository.findDepartmentsForUserInBranch(userId, branchId);

        if (departments.isEmpty()) {
            rollcallService.recordLoginRollcall(userId, null, branchId);
        } else {
            for (Department d : departments) {
                rollcallService.recordLoginRollcall(userId, d.getId(), branchId);
            }
        }

        /* ---------- 6️⃣ Build SAFE response ---------- */
        AuthResponse response = new AuthResponse(
                userId,
                user.getUsername(),
                user.getRole().name(),
                branchId,
                departments.stream().map(Department::getId).toList()
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
        logout(jwtUtil.extractTokenId(token), false);
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

        System.out.println("Hello");
        System.out.println(branchRepository.findById(session.getBranchId()).get().getName());

        return new AuthResponse(
                userId,
                user.getUsername(),
                user.getRole().name(),
                session.getBranchId(),
                departments.stream().map(Department::getId).toList()
        );
    }
}