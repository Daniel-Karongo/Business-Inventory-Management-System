package com.IntegrityTechnologies.business_manager.modules.person.function.auth.service;

import com.IntegrityTechnologies.business_manager.exception.UserNotFoundException;
import com.IntegrityTechnologies.business_manager.modules.person.function.auth.dto.AuthRequest;
import com.IntegrityTechnologies.business_manager.modules.person.function.auth.dto.AuthResponse;
import com.IntegrityTechnologies.business_manager.modules.person.function.auth.util.JwtUtil;
import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.repository.BranchRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.department.model.Department;
import com.IntegrityTechnologies.business_manager.modules.person.entity.department.repository.DepartmentRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.User;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.repository.UserRepository;
import com.IntegrityTechnologies.business_manager.modules.person.function.rollcall.model.UserSession;
import com.IntegrityTechnologies.business_manager.modules.person.function.rollcall.repository.UserSessionRepository;
import com.IntegrityTechnologies.business_manager.modules.person.function.rollcall.service.RollcallService;
import lombok.RequiredArgsConstructor;
import org.springframework.security.authentication.*;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Set;
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
       LOGIN
       ===================================================== */

    @Transactional
    public AuthResponse login(AuthRequest request) {

        // 1️⃣ Resolve user
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

        // 2️⃣ Validate branch & membership
        branchRepository.findByIdAndDeletedFalse(branchId)
                .orElseThrow(() -> new RuntimeException("Selected branch does not exist"));

        boolean belongs = branchRepository.findBranchesByUserId(userId)
                .stream()
                .anyMatch(b -> b.getId().equals(branchId));

        if (!belongs) {
            throw new RuntimeException("User is not assigned to this branch");
        }

        // 3️⃣ Enforce session cap (per day)
        int activeCount = userSessionRepository
                .findByUserIdAndLoginDateAndLogoutTimeIsNull(userId, today)
                .size();

        if (activeCount >= MAX_SESSIONS_PER_DAY) {
            throw new IllegalStateException("Maximum active sessions reached for today");
        }

        // 4️⃣ Create session + JWT
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

        // 5️⃣ Record LOGIN rollcall (once/day logic handled inside service)
        List<Department> departments =
                departmentRepository.findDepartmentsForUserInBranch(userId, branchId);

        if (departments.isEmpty()) {
            rollcallService.recordLoginRollcall(userId, null, branchId);
        } else {
            for (Department d : departments) {
                rollcallService.recordLoginRollcall(userId, d.getId(), branchId);
            }
        }

        // 6️⃣ Build response
        return new AuthResponse(
                userId,
                user.getUsername(),
                user.getRole().name(),
                branchId,
                departments.stream().map(Department::getId).toList(),
                jwtUtil.extractExpiration(token).getTime(),
                token
        );
    }

    /* =====================================================
       LOGOUT (single session)
       ===================================================== */

    @Transactional
    public void logout(UUID tokenId, boolean auto) {

        UserSession session = userSessionRepository
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
       LOGOUT ALL SESSIONS
       (password change / admin action / midnight expiry)
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
}