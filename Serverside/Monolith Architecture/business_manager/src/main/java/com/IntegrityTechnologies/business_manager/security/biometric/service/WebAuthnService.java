package com.IntegrityTechnologies.business_manager.security.biometric.service;

import com.IntegrityTechnologies.business_manager.exception.AppSecurityException;
import com.IntegrityTechnologies.business_manager.security.biometric.config.RelyingPartyFactory;
import com.IntegrityTechnologies.business_manager.security.biometric.model.UserBiometric;
import com.IntegrityTechnologies.business_manager.security.biometric.repository.UserBiometricRepository;
import com.IntegrityTechnologies.business_manager.security.device.model.TrustedDevice;
import com.IntegrityTechnologies.business_manager.security.device.repository.TrustedDeviceRepository;
import com.IntegrityTechnologies.business_manager.security.device.service.DeviceSecurityService;
import com.IntegrityTechnologies.business_manager.security.model.SecurityErrorCode;
import com.yubico.webauthn.*;
import com.yubico.webauthn.data.*;
import com.yubico.webauthn.exception.AssertionFailedException;
import com.yubico.webauthn.exception.RegistrationFailedException;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.util.UUID;

@Service
@RequiredArgsConstructor
public class WebAuthnService {

    private final RelyingPartyFactory relyingPartyFactory;
    private final ChallengeService challengeService;
    private final UserBiometricRepository biometricRepository;
    private final RegistrationChallengeService registrationChallengeService;
    private final DeviceSecurityService deviceSecurityService;
    private final TrustedDeviceRepository trustedDeviceRepository;

    @Value("${security.allowed-origin-suffix}")
    private String allowedOriginSuffix;

    private void validateOrigin(String origin) {

        if (origin == null || origin.isBlank()) {
            throw new AppSecurityException(
                    SecurityErrorCode.INVALID_REQUEST,
                    "Missing origin"
            );
        }

        if (!origin.startsWith("https://")) {
            throw new AppSecurityException(
                    SecurityErrorCode.INVALID_REQUEST,
                    "Invalid origin scheme"
            );
        }

        try {
            java.net.URI uri = java.net.URI.create(origin);
            String host = uri.getHost();

            if (host == null) {
                throw new AppSecurityException(
                        SecurityErrorCode.INVALID_REQUEST,
                        "Invalid origin host"
                );
            }

            String rootHost =
                    allowedOriginSuffix.startsWith(".")
                            ? allowedOriginSuffix.substring(1)
                            : allowedOriginSuffix;

            if (!host.equals(rootHost) && !host.endsWith(allowedOriginSuffix)) {
                throw new AppSecurityException(
                        SecurityErrorCode.INVALID_REQUEST,
                        "Invalid origin domain"
                );
            }

        } catch (Exception e) {
            throw new AppSecurityException(
                    SecurityErrorCode.INVALID_REQUEST,
                    "Invalid origin format"
            );
        }
    }

    /* ================= ASSERTION ================= */

    public AssertionRequest startAssertion(UUID tenantId, String deviceId, String origin) {

        validateOrigin(origin);

        RelyingParty rp = relyingPartyFactory.forOrigin(origin);

        boolean hasBiometric =
                !biometricRepository
                        .findByTenantIdAndDeviceIdAndDeletedFalse(tenantId, deviceId)
                        .isEmpty();

        if (!hasBiometric) {
            throw new AppSecurityException(
                    SecurityErrorCode.BIOMETRIC_CREDENTIAL_NOT_FOUND,
                    "No biometrics registered for this device"
            );
        }

        AssertionRequest request = rp.startAssertion(
                StartAssertionOptions.builder()
                        .userVerification(UserVerificationRequirement.PREFERRED)
                        .build()
        );

        challengeService.store(tenantId, deviceId, request);

        return request;
    }

    public AssertionResult finishAssertion(
            UUID tenantId,
            String deviceId,
            PublicKeyCredential<AuthenticatorAssertionResponse, ClientAssertionExtensionOutputs> credential,
            String origin
    ) {

        validateOrigin(origin);

        WebAuthnRequestContext.setDeviceId(deviceId); // 🔥 ADD THIS

        try {

            RelyingParty rp = relyingPartyFactory.forOrigin(origin);

            AssertionRequest request = challengeService.get(tenantId, deviceId);

            AssertionResult result = rp.finishAssertion(
                    FinishAssertionOptions.builder()
                            .request(request)
                            .response(credential)
                            .build()
            );

            if (!result.isSuccess()) {
                throw new AppSecurityException(
                        SecurityErrorCode.BIOMETRIC_VERIFICATION_FAILED,
                        "WebAuthn verification failed"
                );
            }

            String credentialId = result.getCredentialId().getBase64Url();

            UUID tenantIdCtx =
                    com.IntegrityTechnologies.business_manager.security.util.TenantContext.getTenantId();

            UserBiometric biometric = biometricRepository
                    .findByTenantIdAndCredentialId(tenantIdCtx, credentialId)
                    .orElseThrow(() -> new AppSecurityException(
                            SecurityErrorCode.BIOMETRIC_CREDENTIAL_NOT_FOUND,
                            "Credential not registered for this tenant/device"
                    ));

            if (!biometric.getDeviceId().equals(deviceId)) {
                throw new AppSecurityException(
                        SecurityErrorCode.BIOMETRIC_DEVICE_MISMATCH,
                        "Biometric belongs to a different device"
                );
            }

            UUID userIdFromHandle = UUID.fromString(
                    new String(result.getUserHandle().getBytes())
            );

            if (!biometric.getUserId().equals(userIdFromHandle)) {
                throw new AppSecurityException(
                        SecurityErrorCode.BIOMETRIC_USER_MISMATCH,
                        "Biometric does not belong to this user"
                );
            }

            long newCount = result.getSignatureCount();

            if (newCount > biometric.getSignCount()) {
                biometric.setSignCount(newCount);
                biometricRepository.save(biometric);
            }

            return result;

        } catch (AssertionFailedException e) {

            String credentialId = credential.getId().getBase64Url();

            UUID tenantIdCtx =
                    com.IntegrityTechnologies.business_manager.security.util.TenantContext.getTenantId();

            boolean existsInAnyTenant =
                    biometricRepository.findByCredentialId(credentialId).isPresent();

            boolean existsInThisTenant =
                    biometricRepository.findByTenantIdAndCredentialId(tenantIdCtx, credentialId).isPresent();

            if (!existsInAnyTenant) {
                throw new AppSecurityException(
                        SecurityErrorCode.BIOMETRIC_CREDENTIAL_NOT_FOUND,
                        "Credential not registered"
                );
            }

            if (!existsInThisTenant) {
                throw new AppSecurityException(
                        SecurityErrorCode.BIOMETRIC_TENANT_MISMATCH,
                        "Credential not registered for this tenant/device"
                );
            }

            throw new AppSecurityException(
                    SecurityErrorCode.BIOMETRIC_VERIFICATION_FAILED,
                    "Biometric verification failed"
            );
        } finally {
            WebAuthnRequestContext.clear();
        }
    }

    /* ================= REGISTRATION ================= */

    public PublicKeyCredentialCreationOptions startRegistration(
            UUID tenantId,
            UUID userId,
            UUID branchId,
            String username,
            String deviceId,
            String origin
    ) {
        validateOrigin(origin);

        RelyingParty rp = relyingPartyFactory.forOrigin(origin);

        UserIdentity user = UserIdentity.builder()
                .name(username)
                .displayName(username)
                .id(new ByteArray(userId.toString().getBytes()))
                .build();

        PublicKeyCredentialCreationOptions options =
                rp.startRegistration(
                        StartRegistrationOptions.builder()
                                .user(user)

                                // 🔥 CRITICAL FIX
                                .authenticatorSelection(
                                        AuthenticatorSelectionCriteria.builder()
                                                .residentKey(ResidentKeyRequirement.REQUIRED)
                                                .userVerification(UserVerificationRequirement.PREFERRED)
                                                .build()
                                )

                                .extensions(
                                        RegistrationExtensionInputs.builder()
                                                .credProps()
                                                .build()
                                )
                                .build()
                );

        registrationChallengeService.store(
                tenantId,
                deviceId,
                userId,
                branchId,
                options
        );

        return options;
    }

    public void finishRegistration(
            UUID tenantId,
            String deviceId,
            PublicKeyCredential<AuthenticatorAttestationResponse, ClientRegistrationExtensionOutputs> credential,
            String origin
    ) {
        validateOrigin(origin);

        RelyingParty rp = relyingPartyFactory.forOrigin(origin);

        var ctx = registrationChallengeService.getFull(tenantId, deviceId);

        UUID userId = ctx.userId();

        deviceSecurityService.enforceDeviceLimit(
                tenantId,
                userId,
                ctx.branchId()
        );

        RegistrationResult result;

        try {
            result = rp.finishRegistration(
                    FinishRegistrationOptions.builder()
                            .request(ctx.options())
                            .response(credential)
                            .build()
            );
        } catch (RegistrationFailedException e) {
            throw new AppSecurityException(
                    SecurityErrorCode.BIOMETRIC_INVALID_PAYLOAD,
                    "Biometric registration failed"
            );
        }

        String credentialId = credential.getId().getBase64Url();

        biometricRepository
                .findByTenantIdAndCredentialId(tenantId, credentialId)
                .ifPresent(b -> {
                    throw new AppSecurityException(
                            SecurityErrorCode.INVALID_REQUEST,
                            "Biometric already registered"
                    );
                });

        biometricRepository.save(
                UserBiometric.builder()
                        .userId(userId)
                        .tenantId(tenantId)
                        .credentialId(credentialId)
                        .publicKey(result.getPublicKeyCose().getBase64())
                        .signCount(result.getSignatureCount())
                        .deviceId(deviceId)
                        .deviceName(
                                trustedDeviceRepository
                                        .findByTenantIdAndDeviceIdAndBranchIdIsNull(
                                                tenantId,
                                                deviceId
                                        )
                                        .map(
                                                TrustedDevice::getDeviceName
                                        )
                                        .orElse("Passkey Device")
                        )
                        .build()
        );
    }
}