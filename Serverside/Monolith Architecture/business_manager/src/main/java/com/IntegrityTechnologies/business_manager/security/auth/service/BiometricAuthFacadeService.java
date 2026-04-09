package com.IntegrityTechnologies.business_manager.security.auth.service;

import com.IntegrityTechnologies.business_manager.modules.platform.identity.repository.PlatformUserRepository;
import com.IntegrityTechnologies.business_manager.security.audit.service.LoginAuditService;
import com.IntegrityTechnologies.business_manager.security.auth.dto.AuthRequest;
import com.IntegrityTechnologies.business_manager.security.auth.platform.PlatformAuthService;
import com.IntegrityTechnologies.business_manager.security.auth.tenant.TenantAuthService;
import com.IntegrityTechnologies.business_manager.security.biometric.dto.WebAuthnVerifyRequest;
import com.IntegrityTechnologies.business_manager.security.biometric.service.WebAuthnService;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import com.yubico.webauthn.data.AuthenticatorAssertionResponse;
import com.yubico.webauthn.data.ByteArray;
import com.yubico.webauthn.data.ClientAssertionExtensionOutputs;
import com.yubico.webauthn.data.PublicKeyCredential;
import jakarta.servlet.http.HttpServletRequest;
import lombok.RequiredArgsConstructor;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.stereotype.Service;

import java.nio.charset.StandardCharsets;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class BiometricAuthFacadeService {

    private final WebAuthnService webAuthnService;
    private final AuthService authService;
    private final LoginAuditService loginAuditService;
    private final PlatformAuthService platformAuthService;
    private final TenantAuthService tenantAuthService;
    private final PlatformUserRepository platformUserRepository;

    public AuthService.LoginResult biometricLogin(
            WebAuthnVerifyRequest request,
            HttpServletRequest servletRequest
    ) {

        UUID tenantId = TenantContext.getTenantId();

        String ip = servletRequest.getRemoteAddr();

        PublicKeyCredential<AuthenticatorAssertionResponse, ClientAssertionExtensionOutputs> credential;

        try {
            credential = PublicKeyCredential.parseAssertionResponseJson(request.getRawJson());
        } catch (Exception e) {

            loginAuditService.log(
                    tenantId,
                    null,
                    request.getBranchId(),
                    request.getDeviceId(),
                    request.getLatitude(),
                    request.getLongitude(),
                    request.getAccuracy(),
                    ip,
                    "BLOCKED",
                    "INVALID_BIOMETRIC_PAYLOAD"
            );

            throw new BadCredentialsException("Invalid biometric payload", e);
        }

        try {

            /* ================= 1️⃣ VERIFY ================= */

            var assertionResult  = webAuthnService.finishAssertion(
                    tenantId,
                    request.getDeviceId(),
                    credential
            );

            /* ================= 2️⃣ RESOLVE USER ================= */

            // 🔥 REFACTOR: derive directly (no extra lookup needed)
            ByteArray userHandle = assertionResult .getCredential().getUserHandle();

            if (userHandle == null) {
                throw new SecurityException("Missing user handle");
            }

            UUID userId;

            try {
                userId = UUID.fromString(
                        new String(userHandle.getBytes(), StandardCharsets.UTF_8)
                );
            } catch (Exception e) {
                throw new SecurityException("Invalid user handle format");
            }

            /* ================= 3️⃣ BUILD AUTH REQUEST ================= */

            AuthRequest authRequest = new AuthRequest();

            authRequest.setUserId(userId);
            authRequest.setBranchId(request.getBranchId());
            authRequest.setDeviceId(request.getDeviceId());
            authRequest.setLatitude(request.getLatitude());
            authRequest.setLongitude(request.getLongitude());
            authRequest.setAccuracy(request.getAccuracy());

            /* ================= 4️⃣ LOGIN ================= */

            // 🔍 Determine user type

            boolean isPlatformUser =
                    platformUserRepository.findById(userId).isPresent();

            if (isPlatformUser) {

                var platformLoginResult  = platformAuthService.biometricLogin(authRequest, servletRequest);

                return new AuthService.LoginResult(
                        platformLoginResult .jwt(),
                        platformLoginResult .response()
                );
            }

            // 👉 fallback = tenant
            var tenantLoginResult  = tenantAuthService.biometricLogin(authRequest, servletRequest);

            return new AuthService.LoginResult(
                    tenantLoginResult .jwt(),
                    tenantLoginResult .response()
            );

        } catch (Exception ex) {

            loginAuditService.log(
                    tenantId,
                    null,
                    request.getBranchId(),
                    request.getDeviceId(),
                    request.getLatitude(),
                    request.getLongitude(),
                    request.getAccuracy(),
                    ip,
                    "BLOCKED",
                    "BIOMETRIC_FAILED: " + ex.getMessage()
            );

            throw new BadCredentialsException("Biometric authentication failed");
        }
    }
}