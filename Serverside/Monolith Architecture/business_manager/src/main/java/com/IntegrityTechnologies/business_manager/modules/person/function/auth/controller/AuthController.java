package com.IntegrityTechnologies.business_manager.modules.person.function.auth.controller;

import com.IntegrityTechnologies.business_manager.modules.person.function.auth.dto.AuthRequest;
import com.IntegrityTechnologies.business_manager.modules.person.function.auth.dto.AuthResponse;
import com.IntegrityTechnologies.business_manager.modules.person.function.auth.dto.UserSessionDTO;
import com.IntegrityTechnologies.business_manager.modules.person.function.auth.service.AuthService;
import com.IntegrityTechnologies.business_manager.modules.person.function.auth.util.JwtUtil;
import com.IntegrityTechnologies.business_manager.modules.person.function.rollcall.repository.UserSessionRepository;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.servlet.http.HttpServletRequest;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

@Tag(name = "Auth")
@RestController
@RequestMapping("/api/auth")
public class AuthController {

    private final AuthService authService;
    private final JwtUtil jwtUtil;
    private final UserSessionRepository userSessionRepository;

    @Autowired
    public AuthController(
            AuthService authService,
            JwtUtil jwtUtil,
            UserSessionRepository userSessionRepository
    ) {
        this.authService = authService;
        this.jwtUtil = jwtUtil;
        this.userSessionRepository = userSessionRepository;
    }

    @PostMapping("/login")
    public ResponseEntity<AuthResponse> login(@RequestBody AuthRequest request) {
        return ResponseEntity.ok(authService.login(request));
    }

    @PostMapping("/login/bulk")
    public ResponseEntity<List<AuthResponse>> bulkLogin(@RequestBody List<AuthRequest> requests) {
        List<AuthResponse> responses = new ArrayList<>();
        for(AuthRequest request: requests) {
            responses.add(authService.login(request));
        }
        return ResponseEntity.ok(responses);
    }

    @PostMapping("/logout")
    public ResponseEntity<String> logout(HttpServletRequest request) {

        final String authHeader = request.getHeader("Authorization");
        if (authHeader == null || !authHeader.startsWith("Bearer ")) {
            return ResponseEntity.badRequest().body("No valid Authorization header found");
        }

        String jwt = authHeader.substring(7);

        UUID tokenId = jwtUtil.extractTokenId(jwt);

        authService.logout(tokenId, false); // manual logout

        return ResponseEntity.ok("Successfully logged out");
    }

    @PostMapping("/logout-all")
    public ResponseEntity<String> logoutAll(HttpServletRequest request) {

        final String authHeader = request.getHeader("Authorization");
        if (authHeader == null || !authHeader.startsWith("Bearer ")) {
            return ResponseEntity.badRequest().body("No valid Authorization header found");
        }

        String jwt = authHeader.substring(7);

        UUID userId = jwtUtil.extractUserId(jwt);

        authService.logoutAllSessions(userId, false); // manual logout

        return ResponseEntity.ok("All sessions logged out successfully");
    }

    @PostMapping("/logout-all/{userId}")
    @PreAuthorize("hasAnyRole('SUPERUSER', 'ADMIN', 'MANAGER')")
    public ResponseEntity<String> logoutAllForUser(@PathVariable UUID userId) {

        authService.logoutAllSessions(userId, true); // auto logout

        return ResponseEntity.ok("User logged out from all sessions");
    }

    @PostMapping("/sessions")
    public ResponseEntity<List<UserSessionDTO>> getMySessions(HttpServletRequest request) {

        String authHeader = request.getHeader("Authorization");
        if (authHeader == null || !authHeader.startsWith("Bearer ")) {
            return ResponseEntity.badRequest().build();
        }

        String jwt = authHeader.substring(7);

        UUID userId = jwtUtil.extractUserId(jwt);
        UUID currentTokenId = jwtUtil.extractTokenId(jwt);

        List<UserSessionDTO> sessions =
                userSessionRepository.findAllByUserIdAndLogoutTimeIsNull(userId)
                        .stream()
                        .map(s -> new UserSessionDTO(
                                s.getTokenId(),
                                s.getBranchId(),
                                s.getLoginDate(),
                                s.getLoginTime(),
                                s.getTokenId().equals(currentTokenId)
                        ))
                        .toList();

        return ResponseEntity.ok(sessions);
    }

}