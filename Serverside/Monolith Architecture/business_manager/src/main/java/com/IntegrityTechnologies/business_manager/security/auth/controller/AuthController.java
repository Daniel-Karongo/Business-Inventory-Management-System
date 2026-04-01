package com.IntegrityTechnologies.business_manager.security.auth.controller;

import com.IntegrityTechnologies.business_manager.modules.person.function.rollcall.repository.UserSessionRepository;
import com.IntegrityTechnologies.business_manager.security.auth.dto.AuthRequest;
import com.IntegrityTechnologies.business_manager.security.auth.dto.AuthResponse;
import com.IntegrityTechnologies.business_manager.security.auth.dto.BulkAuthResponse;
import com.IntegrityTechnologies.business_manager.security.auth.dto.UserSessionDTO;
import com.IntegrityTechnologies.business_manager.security.auth.service.AuthService;
import com.IntegrityTechnologies.business_manager.security.auth.util.JwtUtil;
import com.IntegrityTechnologies.business_manager.security.biometric.dto.WebAuthnVerifyRequest;
import com.IntegrityTechnologies.business_manager.security.biometric.service.WebAuthnService;
import com.yubico.webauthn.AssertionRequest;
import com.yubico.webauthn.data.AuthenticatorAssertionResponse;
import com.yubico.webauthn.data.ClientAssertionExtensionOutputs;
import com.yubico.webauthn.data.PublicKeyCredential;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.servlet.http.Cookie;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.authentication.BadCredentialsException;
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
    private final WebAuthnService webAuthnService;

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
        cookie.setHttpOnly(true);
        cookie.setSecure(false);
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

    @PostMapping("/biometric/challenge")
    public ResponseEntity<?> challenge(@RequestParam String deviceId) {

        AssertionRequest request = webAuthnService.startAssertion(deviceId);

        return ResponseEntity.ok(request);
    }

    @PostMapping("/biometric/verify")
    public ResponseEntity<AuthResponse> biometricLogin(
            @RequestBody WebAuthnVerifyRequest request,
            HttpServletRequest servletRequest,
            HttpServletResponse response
    ) {

        PublicKeyCredential<AuthenticatorAssertionResponse, ClientAssertionExtensionOutputs> credential;

        try {
            credential = PublicKeyCredential.parseAssertionResponseJson(request.getRawJson());
        } catch (Exception e) {
            throw new BadCredentialsException("Invalid biometric payload", e);
        }

    /* =====================================================
       1️⃣ VERIFY WEBAUTHN
    ===================================================== */

        var result = webAuthnService.finishAssertion(request.getDeviceId(), credential);

    /* =====================================================
       2️⃣ RESOLVE USER VIA CREDENTIAL ID (CORRECT WAY)
    ===================================================== */

        String credentialId = credential.getId().getBase64Url();

        UUID userId = webAuthnService.resolveUserId(credentialId);

    /* =====================================================
       3️⃣ BUILD AUTH REQUEST
    ===================================================== */

        AuthRequest authRequest = new AuthRequest();

        authRequest.setUserId(userId);
        authRequest.setBranchId(request.getBranchId());
        authRequest.setDeviceId(request.getDeviceId());
        authRequest.setLatitude(request.getLatitude());
        authRequest.setLongitude(request.getLongitude());
        authRequest.setAccuracy(request.getAccuracy());

    /* =====================================================
       4️⃣ LOGIN (BIOMETRIC PATH)
    ===================================================== */

        AuthService.LoginResult loginResult =
                authService.loginInternalBiometric(authRequest, servletRequest);

    /* =====================================================
       5️⃣ COOKIE
    ===================================================== */

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
    
    /* =====================================================
       BULK LOGIN (NON-BROWSER / ADMIN / AUTOMATION)
       ===================================================== */
    @PostMapping("/login/bulk")
    @PreAuthorize("hasAnyRole('SUPERUSER','ADMIN')")
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

        cookie.setHttpOnly(true);
        cookie.setSecure(false);
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