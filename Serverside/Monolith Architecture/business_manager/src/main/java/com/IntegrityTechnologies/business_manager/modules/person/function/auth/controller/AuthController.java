package com.IntegrityTechnologies.business_manager.modules.person.function.auth.controller;

import com.IntegrityTechnologies.business_manager.modules.person.function.auth.dto.*;
import com.IntegrityTechnologies.business_manager.modules.person.function.auth.service.AuthService;
import com.IntegrityTechnologies.business_manager.modules.person.function.auth.util.JwtUtil;
import com.IntegrityTechnologies.business_manager.modules.person.function.rollcall.repository.UserSessionRepository;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.servlet.http.Cookie;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

@Tag(name = "Auth")
@RestController
@RequestMapping("/api/auth")
@RequiredArgsConstructor
public class AuthController {

    private final AuthService authService;
    private final JwtUtil jwtUtil;
    private final UserSessionRepository userSessionRepository;

    /* =====================================================
       LOGIN (BROWSER – HttpOnly COOKIE)
       ===================================================== */
    @PostMapping("/login")
    public ResponseEntity<AuthResponse> login(
            @RequestBody AuthRequest request,
            HttpServletResponse response,
            HttpServletRequest servletRequest
    ) {

        AuthService.LoginResult result = authService.loginInternal(request);

        boolean isSecure = servletRequest.isSecure();

        Cookie cookie = new Cookie("access_token", result.jwt());
        cookie.setHttpOnly(true);
        cookie.setSecure(isSecure);
        cookie.setPath("/");
        cookie.setMaxAge((int) jwtUtil.secondsUntilMidnight());

        if (isSecure) {
            cookie.setAttribute("SameSite", "None");
        } else {
            cookie.setAttribute("SameSite", "Lax");
        }

        response.addCookie(cookie);

        return ResponseEntity.ok(result.response());
    }

    /* =====================================================
       BULK LOGIN (NON-BROWSER / ADMIN / AUTOMATION)
       ===================================================== */
    @PostMapping("/login/bulk")
    @PreAuthorize("hasAnyRole('SUPERUSER','ADMIN')")
    public ResponseEntity<List<BulkAuthResponse>> bulkLogin(
            @RequestBody List<AuthRequest> requests
    ) {
        List<BulkAuthResponse> responses = new ArrayList<>();

        for (AuthRequest request : requests) {
            AuthService.LoginResult result = authService.loginInternal(request);

            responses.add(
                    new BulkAuthResponse(
                            result.response().getUserId(),
                            result.response().getUsername(),
                            result.response().getRole(),
                            result.response().getBranchId(),
                            result.jwt(),
                            jwtUtil.extractExpiration(result.jwt()).getTime()
                    )
            );
        }

        return ResponseEntity.ok(responses);
    }

    /* =====================================================
       LOGOUT (CURRENT SESSION)
       ===================================================== */
    @PostMapping("/logout")
    public ResponseEntity<Void> logout(
            @CookieValue(name = "access_token", required = false) String token,
            HttpServletRequest request,
            HttpServletResponse response
    ) {

        if (token != null) {
            authService.logoutByToken(token);
        }

        boolean isSecure = request.isSecure();

        Cookie cookie = new Cookie("access_token", "");
        cookie.setHttpOnly(true);
        cookie.setSecure(isSecure);
        cookie.setPath("/");
        cookie.setMaxAge(0);

        if (isSecure) {
            cookie.setAttribute("SameSite", "None");
        } else {
            cookie.setAttribute("SameSite", "Lax");
        }

        response.addCookie(cookie);

        return ResponseEntity.ok().build();
    }

    /* =====================================================
       LOGOUT ALL (SELF)
       ===================================================== */
    @PostMapping("/logout-all")
    public ResponseEntity<Void> logoutAll(
            @CookieValue("access_token") String token
    ) {
        UUID userId = jwtUtil.extractUserId(token);
        authService.logoutAllSessions(userId, false);
        return ResponseEntity.ok().build();
    }

    /* =====================================================
       LOGOUT ALL (ADMIN)
       ===================================================== */
    @PostMapping("/logout-all/{userId}")
    @PreAuthorize("hasAnyRole('SUPERUSER','ADMIN','MANAGER')")
    public ResponseEntity<Void> logoutAllForUser(@PathVariable UUID userId) {
        authService.logoutAllSessions(userId, true);
        return ResponseEntity.ok().build();
    }

    /* =====================================================
       ACTIVE SESSIONS (CURRENT USER)
       ===================================================== */
    @PostMapping("/sessions")
    public ResponseEntity<List<UserSessionDTO>> mySessions(
            @CookieValue("access_token") String token
    ) {
        UUID userId = jwtUtil.extractUserId(token);
        UUID currentTokenId = jwtUtil.extractTokenId(token);

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

    /* =====================================================
       CURRENT USER (TOPBAR / APP INIT)
       ===================================================== */
    @GetMapping("/me")
    public ResponseEntity<AuthResponse> me(
            @CookieValue("access_token") String token
    ) {
        return ResponseEntity.ok(authService.resolveMe(token));
    }
}