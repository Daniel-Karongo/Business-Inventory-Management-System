package com.IntegrityTechnologies.business_manager.security.auth.service;

import com.IntegrityTechnologies.business_manager.modules.platform.identity.repository.PlatformUserRepository;
import com.IntegrityTechnologies.business_manager.security.audit.service.LoginAuditService;
import com.IntegrityTechnologies.business_manager.security.auth.dto.AuthRequest;
import com.IntegrityTechnologies.business_manager.security.auth.platform.PlatformAuthService;
import com.IntegrityTechnologies.business_manager.security.auth.tenant.TenantAuthService;
import com.IntegrityTechnologies.business_manager.security.biometric.repository.UserBiometricRepository;
import com.IntegrityTechnologies.business_manager.security.biometric.service.WebAuthnService;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.yubico.webauthn.data.AuthenticatorAssertionResponse;
import com.yubico.webauthn.data.ClientAssertionExtensionOutputs;
import com.yubico.webauthn.data.PublicKeyCredential;
import jakarta.servlet.http.HttpServletRequest;
import lombok.RequiredArgsConstructor;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.stereotype.Service;

import java.util.UUID;

@Service
@RequiredArgsConstructor
public class BiometricAuthFacadeService {

    private final WebAuthnService webAuthnService;
    private final LoginAuditService loginAuditService;
    private final PlatformAuthService platformAuthService;
    private final TenantAuthService tenantAuthService;
    private final PlatformUserRepository platformUserRepository;
    private final UserBiometricRepository biometricRepository;
    private final ObjectMapper objectMapper;

    public AuthService.LoginResult biometricLogin(
            PublicKeyCredential<AuthenticatorAssertionResponse, ClientAssertionExtensionOutputs> credential,
            String deviceId,
            UUID branchId,
            Double latitude,
            Double longitude,
            Double accuracy,
            HttpServletRequest servletRequest,
            String origin
    ) {

        UUID tenantId = TenantContext.getTenantId();
        String ip = servletRequest.getRemoteAddr();

        if (credential == null) {
            loginAuditService.log(
                    tenantId,
                    null,
                    branchId,
                    deviceId,
                    latitude,
                    longitude,
                    accuracy,
                    ip,
                    "BLOCKED",
                    "INVALID_BIOMETRIC_PAYLOAD"
            );
            throw new BadCredentialsException("Invalid biometric payload");
        }

        System.out.println("Hello");
        try {

            /* ================= 1️⃣ VERIFY ================= */

            var assertionResult = webAuthnService.finishAssertion(
                    tenantId,
                    deviceId,
                    credential,
                    origin
            );

            System.out.println("assertionResult: ");
            System.out.println(assertionResult);
            /* ================= 2️⃣ RESOLVE USER ================= */

            String credentialId = assertionResult.getCredentialId().getBase64Url();

            System.out.println("credentialId: ");
            System.out.println(credentialId);

            UUID userId = biometricRepository
                    .findByCredentialId(credentialId)
                    .map(b -> b.getUserId())
                    .orElseThrow(() -> new SecurityException("User not found"));

            /* ================= 3️⃣ BUILD AUTH REQUEST ================= */

            AuthRequest authRequest = new AuthRequest();

            authRequest.setUserId(userId);
            authRequest.setBranchId(branchId);
            authRequest.setDeviceId(deviceId);
            authRequest.setLatitude(latitude);
            authRequest.setLongitude(longitude);
            authRequest.setAccuracy(accuracy);

            /* ================= 4️⃣ LOGIN ================= */

            System.out.println("USER ID: " + userId);
            System.out.println("IS PLATFORM USER: " +
                    platformUserRepository.findById(userId).isPresent());

            boolean isPlatformUser =
                    platformUserRepository.findById(userId).isPresent();

            if (isPlatformUser) {

                var result = platformAuthService.biometricLogin(authRequest, servletRequest);

                return new AuthService.LoginResult(
                        result.jwt(),
                        result.response()
                );
            }

            var result = tenantAuthService.biometricLogin(authRequest, servletRequest);

            return new AuthService.LoginResult(
                    result.jwt(),
                    result.response()
            );

        } catch (Exception ex) {
            ex.printStackTrace();

            String reason = ex.getMessage();
            if (reason != null && reason.length() > 255) {
                reason = reason.substring(0, 255);
            }

            loginAuditService.log(
                    tenantId,
                    null,
                    branchId,
                    deviceId,
                    latitude,
                    longitude,
                    accuracy,
                    ip,
                    "BLOCKED",
                    "BIOMETRIC_FAILED: " + reason
            );

            throw new BadCredentialsException("Biometric authentication failed", ex);
        }
    }
}