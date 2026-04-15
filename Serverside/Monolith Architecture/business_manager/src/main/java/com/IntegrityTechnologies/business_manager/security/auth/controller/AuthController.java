package com.IntegrityTechnologies.business_manager.security.auth.controller;

import com.IntegrityTechnologies.business_manager.modules.person.system.rollcall.repository.UserSessionRepository;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.PublicEndpoint;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantAdminOnly;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantManagerOnly;
import com.IntegrityTechnologies.business_manager.security.auth.dto.AuthRequest;
import com.IntegrityTechnologies.business_manager.security.auth.dto.AuthResponse;
import com.IntegrityTechnologies.business_manager.security.auth.dto.BulkAuthResponse;
import com.IntegrityTechnologies.business_manager.security.auth.dto.UserSessionDTO;
import com.IntegrityTechnologies.business_manager.security.auth.orchestrator.AuthOrchestratorService;
import com.IntegrityTechnologies.business_manager.security.auth.service.AuthService;
import com.IntegrityTechnologies.business_manager.security.auth.service.BiometricAuthFacadeService;
import com.IntegrityTechnologies.business_manager.security.auth.util.JwtUtil;
import com.IntegrityTechnologies.business_manager.security.biometric.dto.BiometricStartRequest;
import com.IntegrityTechnologies.business_manager.security.biometric.dto.WebAuthnVerifyRequest;
import com.IntegrityTechnologies.business_manager.security.biometric.service.WebAuthnService;
import com.IntegrityTechnologies.business_manager.security.device.service.DeviceSecurityService;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.yubico.webauthn.AssertionRequest;
import com.yubico.webauthn.data.AuthenticatorAttestationResponse;
import com.yubico.webauthn.data.ClientRegistrationExtensionOutputs;
import com.yubico.webauthn.data.PublicKeyCredential;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.servlet.http.Cookie;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.web.bind.annotation.*;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.UUID;

@Tag(name = "Auth")
@RestController
@RequestMapping("/api/auth")
@RequiredArgsConstructor
@PublicEndpoint
public class AuthController {

    private final AuthOrchestratorService authOrchestrator;
    private final AuthService authService;
    private final JwtUtil jwtUtil;
    private final UserSessionRepository userSessionRepository;
    private final WebAuthnService webAuthnService;
    private final DeviceSecurityService deviceSecurityService;
    private final BiometricAuthFacadeService biometricAuthFacadeService;
    private final ObjectMapper objectMapper;

    /* =====================================================
       LOGIN (BROWSER – HttpOnly COOKIE)
       ===================================================== */
    @PostMapping("/login")
    public ResponseEntity<AuthResponse> login(
            @RequestBody AuthRequest request,
            HttpServletResponse response,
            HttpServletRequest servletRequest
    ) {

        AuthService.LoginResult result =
                authOrchestrator.login(request, servletRequest);

        boolean isSecure = servletRequest.isSecure();

        Cookie cookie = new Cookie("access_token", result.jwt());

        cookie.setSecure(isSecure);
        cookie.setHttpOnly(true);
        cookie.setPath("/");
        cookie.setMaxAge((int) jwtUtil.secondsUntilMidnight());

        cookie.setAttribute("SameSite", isSecure ? "None" : "Lax");

        response.addCookie(cookie);

        return ResponseEntity.ok(result.response());
    }

    @PostMapping("/biometric/challenge")
    public ResponseEntity<?> challenge(
            @RequestParam String deviceId,
            HttpServletRequest request
    ) {
        UUID tenantId = TenantContext.getTenantId();
        String origin = request.getHeader("Origin");

        if (origin == null) {
            throw new RuntimeException("Missing Origin header");
        }

        AssertionRequest assertion =
                webAuthnService.startAssertion(tenantId, deviceId, origin);

        return ResponseEntity.ok(assertion);
    }

    @PostMapping("/biometric/verify")
    public ResponseEntity<AuthResponse> biometricLogin(
            @RequestBody WebAuthnVerifyRequest request,
            HttpServletRequest servletRequest,
            HttpServletResponse response
    ) {

        String origin = servletRequest.getHeader("Origin");

        if (origin == null) {
            throw new RuntimeException("Missing Origin header");
        }

        AuthService.LoginResult loginResult =
                biometricAuthFacadeService.biometricLogin(request, servletRequest, origin);

        Cookie cookie = new Cookie("access_token", loginResult.jwt());

        cookie.setHttpOnly(true);
        cookie.setSecure(servletRequest.isSecure());
        cookie.setPath("/");
        cookie.setMaxAge((int) jwtUtil.secondsUntilMidnight());
        cookie.setAttribute("SameSite",
                servletRequest.isSecure() ? "None" : "Lax");

        response.addCookie(cookie);

        return ResponseEntity.ok(loginResult.response());
    }

    @PostMapping("/biometric/register/start")
    public ResponseEntity<?> initRegistration(
            @RequestBody BiometricStartRequest request,
            HttpServletRequest servletRequest
    ) {
        UUID tenantId = TenantContext.getTenantId();

        UUID userId = jwtUtil.extractUserIdFromRequest(servletRequest);
        String username = jwtUtil.extractUsernameFromRequest(servletRequest);
        UUID branchId = jwtUtil.extractBranchId(jwtUtil.extractTokenFromRequest(servletRequest));

        String deviceId = request.deviceId();

        deviceSecurityService.validate(tenantId, branchId, deviceId);

        String origin = servletRequest.getHeader("Origin");

        if (origin == null) {
            throw new RuntimeException("Missing Origin header");
        }

        var options = webAuthnService.startRegistration(
                tenantId,
                userId,
                username,
                deviceId,
                origin
        );

        Map<String, Object> response =
                objectMapper.convertValue(options, Map.class);

        Map<String, Object> extensions =
                (Map<String, Object>) response.get("extensions");

        if (extensions != null && extensions.get("appidExclude") == null) {
            extensions.remove("appidExclude");
        }

        return ResponseEntity.ok(response);
    }

    @PostMapping("/biometric/register/finish")
    public ResponseEntity<?> finishRegistration(
            @RequestBody Map<String, Object> body,
            @RequestParam String deviceId,
            HttpServletRequest request
    ) {
        UUID tenantId = TenantContext.getTenantId();
        String origin = request.getHeader("Origin");

        if (origin == null) {
            throw new RuntimeException("Missing Origin header");
        }

        String host = request.getServerName(); // platform.local.test

        if (!origin.contains(host)) {
            throw new RuntimeException("Origin mismatch");
        }

        PublicKeyCredential<AuthenticatorAttestationResponse, ClientRegistrationExtensionOutputs> credential;

        try {
            String rawJson = objectMapper.writeValueAsString(body); // ✅ convert back to JSON string
            credential = PublicKeyCredential.parseRegistrationResponseJson(rawJson);
        } catch (Exception e) {
            e.printStackTrace(); // 🔥 ADD THIS
            throw new RuntimeException("Invalid registration payload");
        }

        // ✅ PASS userId
        webAuthnService.finishRegistration(
                tenantId,
                deviceId,
                credential,
                origin
        );

        return ResponseEntity.ok().build();
    }

    /* =====================================================
       BULK LOGIN (NON-BROWSER / ADMIN / AUTOMATION)
       ===================================================== */
    @PostMapping("/login/bulk")
    @TenantAdminOnly
    public ResponseEntity<List<BulkAuthResponse>> bulkLogin(
            @RequestBody List<AuthRequest> requests,
            HttpServletRequest servletRequest
    ) {
        List<BulkAuthResponse> responses = new ArrayList<>();

        for (AuthRequest request : requests) {
            AuthService.LoginResult result =
                    authOrchestrator.login(request, servletRequest);

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

        cookie.setSecure(isSecure);
        cookie.setHttpOnly(true);
        cookie.setPath("/");
        cookie.setMaxAge(0);

        cookie.setAttribute("SameSite", isSecure ? "None" : "Lax");

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
    @TenantManagerOnly
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
    public ResponseEntity<?> me(
            @CookieValue(name = "access_token", required = false) String token
    ) {

        if (token == null || token.isBlank()) {
            return ResponseEntity.status(401).build(); // clean 401
        }

        try {
            return ResponseEntity.ok(authService.resolveMe(token));
        } catch (RuntimeException ex) {
            return ResponseEntity.status(401).build();
        }
    }
}