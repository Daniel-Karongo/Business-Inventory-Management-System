package com.IntegrityTechnologies.business_manager.modules.person.function.auth.service;

import com.IntegrityTechnologies.business_manager.exception.UserNotFoundException;
import com.IntegrityTechnologies.business_manager.modules.person.function.auth.dto.AuthRequest;
import com.IntegrityTechnologies.business_manager.modules.person.function.auth.dto.AuthResponse;
import com.IntegrityTechnologies.business_manager.modules.person.function.auth.util.JwtUtil;
import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.model.Branch;
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

import java.time.LocalDateTime;
import java.util.List;
import java.util.Set;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class AuthService {

    private final AuthenticationManager authenticationManager;
    private final JwtUtil jwtUtil;
    private final UserRepository userRepository;
    private final PasswordEncoder passwordEncoder;
    private final TokenBlacklistService tokenBlacklistService;
    private final RollcallService rollcallService;
    private final DepartmentRepository departmentRepository;
    private final BranchRepository branchRepository;
    private final UserSessionRepository userSessionRepository;

    @Transactional
    public AuthResponse login(AuthRequest request) {

        // ------------------------
        // 1️⃣ Validate credentials
        // ------------------------
        User user = userRepository.findByIdentifier(request.getIdentifier())
                .orElseThrow(() -> new UserNotFoundException("User not found"));

        if (!passwordEncoder.matches(request.getPassword(), user.getPassword())) {
            throw new BadCredentialsException("Invalid password");
        }

        authenticationManager.authenticate(
                new UsernamePasswordAuthenticationToken(
                        user.getUsername(), request.getPassword()
                )
        );

        // ------------------------
        // 2️⃣ Generate JWT Token
        // ------------------------
        String token = jwtUtil.generateToken(
                user.getId(),
                user.getUsername(),
                user.getRole().name()
        );

        UUID userId = user.getId();
        UUID branchId = request.getBranchId();

        // ------------------------
        // 3️⃣ Validate branch
        // ------------------------
        branchRepository.findByIdAndDeletedFalse(branchId)
                .orElseThrow(() -> new RuntimeException("Selected branch does not exist"));

        // ------------------------
        // 4️⃣ Check user belongs to branch
        // ------------------------
        boolean userBelongs = branchRepository.findBranchesByUserId(userId)
                .stream()
                .anyMatch(b -> b.getId().equals(branchId));

        if (!userBelongs) {
            throw new RuntimeException("User is not assigned to this branch");
        }

        // ------------------------
        // 5️⃣ Get departments the user is assigned to in this branch
        // ------------------------
        List<Department> departments =
                departmentRepository.findDepartmentsForUserInBranch(userId, branchId);

        List<UUID> departmentIds = departments.stream()
                .map(Department::getId)
                .toList();

        // ------------------------
        // 6️⃣ Record login rollcall
        // ------------------------
        if (departments.isEmpty()) {
            rollcallService.recordLoginRollcall(userId, null, branchId);
        } else {
            for (Department department : departments) {
                rollcallService.recordLoginRollcall(
                        userId,
                        department.getId(),
                        branchId
                );
            }
        }

        // ------------------------
        // 7️⃣ Save session info
        // ------------------------
        userSessionRepository.save(
                UserSession.builder()
                        .userId(userId)
                        .branchId(branchId)
                        .loginTime(LocalDateTime.now())
                        .build()
        );

        // ------------------------
        // 8️⃣ Build improved AuthResponse
        // ------------------------
        long expiresAt = jwtUtil.extractExpiration(token).getTime();

        return new AuthResponse(
                user.getId(),                  // userId
                user.getUsername(),            // userName
                user.getRole().name(),         // role
                branchId,                      // branchId
                departmentIds,                 // departments
                expiresAt,                     // expiry timestamp
                token                          // JWT
        );
    }

    public void logout(String token) {

        UUID userId = jwtUtil.extractUserId(token);

        UserSession session = userSessionRepository
                .findTopByUserIdAndLogoutTimeIsNullOrderByLoginTimeDesc(userId)
                .orElse(null);

        UUID branchId = null;
        if (session != null) {
            session.setLogoutTime(LocalDateTime.now());
            userSessionRepository.save(session);

            branchId = session.getBranchId();  // ✅ Use the logged-in branch
        }

        Set<Department> departments = departmentRepository.findDepartmentsByUserId(userId);

        if (departments.isEmpty()) {
            rollcallService.recordLogoutRollcall(userId, null, branchId);
        } else {
            for (Department d : departments) {
                rollcallService.recordLogoutRollcall(userId, d.getId(), branchId);
            }
        }

        tokenBlacklistService.blacklistToken(token);
    }

    public boolean isTokenBlacklisted(String token) {
        return tokenBlacklistService.isTokenBlacklisted(token);
    }
}
