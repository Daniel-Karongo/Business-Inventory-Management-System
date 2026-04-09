package com.IntegrityTechnologies.business_manager.security.auth.controller;

import com.IntegrityTechnologies.business_manager.modules.person.system.rollcall.repository.UserSessionRepository;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.PublicEndpoint;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantAdminOnly;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantManagerOnly;
import com.IntegrityTechnologies.business_manager.security.auth.dto.AuthRequest;
import com.IntegrityTechnologies.business_manager.security.auth.dto.AuthResponse;
import com.IntegrityTechnologies.business_manager.security.auth.dto.BulkAuthResponse;
import com.IntegrityTechnologies.business_manager.security.auth.dto.UserSessionDTO;
import com.IntegrityTechnologies.business_manager.security.auth.service.AuthService;
import com.IntegrityTechnologies.business_manager.security.auth.service.BiometricAuthFacadeService;
import com.IntegrityTechnologies.business_manager.security.auth.util.DeviceFingerprintUtil;
import com.IntegrityTechnologies.business_manager.security.auth.util.JwtUtil;
import com.IntegrityTechnologies.business_manager.security.biometric.dto.WebAuthnVerifyRequest;
import com.IntegrityTechnologies.business_manager.security.biometric.service.WebAuthnService;
import com.IntegrityTechnologies.business_manager.security.device.service.DeviceSecurityService;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
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
import java.util.UUID;

@Tag(name = "Auth")
@RestController
@RequestMapping("/api/auth")
@RequiredArgsConstructor
@PublicEndpoint
public class AuthController {

    private final AuthService authService;
    private final JwtUtil jwtUtil;
    private final UserSessionRepository userSessionRepository;
    private final WebAuthnService webAuthnService;
    private final DeviceSecurityService deviceSecurityService;
    private final BiometricAuthFacadeService biometricAuthFacadeService;

    /* =====================================================
       LOGIN (BROWSER – HttpOnly COOKIE)
       ===================================================== */
    @PostMapping("/login")
    public ResponseEntity<AuthResponse> login(
            @RequestBody AuthRequest request,
            HttpServletResponse response,
            HttpServletRequest servletRequest
    ) {

        AuthService.LoginResult result = authService.loginInternal(request, servletRequest);

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

        String fingerprint =
                DeviceFingerprintUtil.generate(request, deviceId);

        AssertionRequest assertion =
                webAuthnService.startAssertion(tenantId, fingerprint);

        return ResponseEntity.ok(assertion);
    }

    @PostMapping("/biometric/verify")
    public ResponseEntity<AuthResponse> biometricLogin(
            @RequestBody WebAuthnVerifyRequest request,
            HttpServletRequest servletRequest,
            HttpServletResponse response
    ) {

        AuthService.LoginResult loginResult =
                biometricAuthFacadeService.biometricLogin(request, servletRequest);

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
    public ResponseEntity<?> startRegistration(
            @RequestBody AuthRequest request,
            HttpServletRequest servletRequest
    ) {
        UUID tenantId = TenantContext.getTenantId();

        // 🔐 Authenticate first (NO SESSION CREATION)
        AuthService.LoginResult result =
                authService.loginInternal(request, servletRequest);

        UUID userId = result.response().getUserId();

        String fingerprint =
                DeviceFingerprintUtil.generate(servletRequest, request.getDeviceId());

        // 🔒 Device rule
        boolean hasAnyDevice =
                deviceSecurityService.hasAnyDeviceForTenantBranch(
                        tenantId,
                        request.getBranchId()
                );

        if (hasAnyDevice) {
            deviceSecurityService.validate(tenantId, request.getBranchId(), fingerprint);
        }

        var options = webAuthnService.startRegistration(
                tenantId,
                userId,
                result.response().getUsername(),
                fingerprint
        );

        return ResponseEntity.ok(options);
    }

    @PostMapping("/biometric/register/finish")
    public ResponseEntity<?> finishRegistration(
            @RequestBody String rawJson,
            @RequestParam String deviceId,
            HttpServletRequest request
    ) {
        UUID tenantId = TenantContext.getTenantId();

        String fingerprint =
                DeviceFingerprintUtil.generate(request, deviceId);

        PublicKeyCredential<AuthenticatorAttestationResponse, ClientRegistrationExtensionOutputs> credential;

        try {
            credential = PublicKeyCredential.parseRegistrationResponseJson(rawJson);
        } catch (Exception e) {
            throw new BadCredentialsException("Invalid registration payload");
        }

        webAuthnService.finishRegistration(
                tenantId,
                fingerprint,
                credential
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
            AuthService.LoginResult result = authService.loginInternal(request, servletRequest);

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
    public ResponseEntity<AuthResponse> me(
            @CookieValue("access_token") String token
    ) {
        return ResponseEntity.ok(authService.resolveMe(token));
    }
}