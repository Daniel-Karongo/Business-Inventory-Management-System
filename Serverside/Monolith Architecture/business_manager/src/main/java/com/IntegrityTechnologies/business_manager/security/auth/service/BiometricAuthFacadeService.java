package com.IntegrityTechnologies.business_manager.security.auth.service;

import com.IntegrityTechnologies.business_manager.exception.AppSecurityException;
import com.IntegrityTechnologies.business_manager.modules.platform.identity.repository.PlatformUserRepository;
import com.IntegrityTechnologies.business_manager.security.audit.service.LoginAuditService;
import com.IntegrityTechnologies.business_manager.security.auth.dto.AuthRequest;
import com.IntegrityTechnologies.business_manager.security.auth.platform.PlatformAuthService;
import com.IntegrityTechnologies.business_manager.security.auth.tenant.TenantAuthService;
import com.IntegrityTechnologies.business_manager.security.biometric.repository.UserBiometricRepository;
import com.IntegrityTechnologies.business_manager.security.biometric.service.WebAuthnService;
import com.IntegrityTechnologies.business_manager.security.model.SecurityErrorCode;
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
            throw new AppSecurityException(
                    SecurityErrorCode.BIOMETRIC_INVALID_PAYLOAD,
                    "Invalid biometric payload"
            );
        }

        try {

            /* ================= 1️⃣ VERIFY ================= */

            var assertionResult = webAuthnService.finishAssertion(
                    tenantId,
                    deviceId,
                    credential,
                    origin
            );

            /* ================= 2️⃣ RESOLVE USER ================= */

            String credentialId = assertionResult.getCredentialId().getBase64Url();

            UUID userId = biometricRepository
                    .findByCredentialId(credentialId)
                    .map(b -> b.getUserId())
                    .orElseThrow(() -> new AppSecurityException(
                            SecurityErrorCode.BIOMETRIC_CREDENTIAL_NOT_FOUND,
                            "Biometric credential not recognized"
                    ));

            /* ================= 3️⃣ BUILD AUTH REQUEST ================= */

            AuthRequest authRequest = new AuthRequest();

            authRequest.setUserId(userId);
            authRequest.setBranchId(branchId);
            authRequest.setDeviceId(deviceId);
            authRequest.setLatitude(latitude);
            authRequest.setLongitude(longitude);
            authRequest.setAccuracy(accuracy);

            /* ================= 4️⃣ LOGIN ================= */

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

        } catch (AppSecurityException ex) {

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
                    ex.getCode().name()
            );

            throw ex; // 🔥 DO NOT WRAP
        } catch (Exception ex) {

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
                    "UNKNOWN"
            );

            throw new AppSecurityException(
                    SecurityErrorCode.UNKNOWN,
                    "Biometric authentication failed"
            );
        }
    }
}