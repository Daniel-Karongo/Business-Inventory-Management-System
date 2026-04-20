package com.IntegrityTechnologies.business_manager.security.biometric.service;

import com.IntegrityTechnologies.business_manager.security.biometric.config.RelyingPartyFactory;
import com.IntegrityTechnologies.business_manager.security.biometric.model.UserBiometric;
import com.IntegrityTechnologies.business_manager.security.biometric.repository.UserBiometricRepository;
import com.IntegrityTechnologies.business_manager.security.device.service.DeviceSecurityService;
import com.yubico.webauthn.*;
import com.yubico.webauthn.data.*;
import com.yubico.webauthn.exception.AssertionFailedException;
import lombok.RequiredArgsConstructor;
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

    private void validateOrigin(String origin) {

        if (origin == null || origin.isBlank()) {
            throw new SecurityException("Missing origin");
        }

        if (!origin.startsWith("https://")) {
            throw new SecurityException("Invalid origin scheme");
        }

        // ✅ allow ALL tenants + platform
        if (!origin.contains(".local.test")) {
            throw new SecurityException("Invalid origin domain");
        }
    }

    /* ================= ASSERTION ================= */

    public AssertionRequest startAssertion(UUID tenantId, String deviceId, String origin) {

        RelyingParty rp = relyingPartyFactory.forOrigin(origin);

        System.out.println("hello start assertion");
        System.out.println(rp);

        // ✅ Validate device has biometrics (fast fail)
        boolean hasBiometric = biometricRepository
                .findByTenantIdAndDeviceIdAndDeletedFalse(tenantId, deviceId)
                .isEmpty() == false;

        if (!hasBiometric) {
            throw new SecurityException("No biometrics registered for this device");
        }

        // ✅ DO NOT set allowCredentials (handled internally by repository)
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

        RelyingParty rp = relyingPartyFactory.forOrigin(origin);

        System.out.println("Hello finish assertion");
        System.out.println(rp);

        AssertionRequest request = challengeService.get(tenantId, deviceId);

        try {
            AssertionResult result = rp.finishAssertion(
                    FinishAssertionOptions.builder()
                            .request(request)
                            .response(credential)
                            .build()
            );

            if (!result.isSuccess()) {
                throw new SecurityException("WebAuthn verification failed");
            }

            String credentialId = result.getCredentialId().getBase64Url();

            System.out.println("STORED credentialId: " + credential.getId().getBase64Url());

            UserBiometric biometric = biometricRepository.findByCredentialId(credentialId)
                    .orElseThrow(() -> new SecurityException("Credential not recognized"));

            if (!biometric.getCredentialId().equals(credential.getId().getBase64Url())) {
                throw new SecurityException("Credential mismatch");
            }

            UUID userId = biometric.getUserId();

            if (!biometric.getDeviceId().equals(deviceId)) {
                throw new SecurityException("Biometric used on wrong device");
            }

            long newCount = result.getSignatureCount();

            if (newCount > biometric.getSignCount()) {
                biometric.setSignCount(newCount);
                biometricRepository.save(biometric);
            }

            if (!biometric.getUserId().equals(userId)) {
                throw new SecurityException("User mismatch");
            }

            return result;

        } catch (AssertionFailedException e) {
            e.printStackTrace();
            throw new SecurityException("WebAuthn verification failed: " + e.getMessage(), e);
        }
    }

    /* ================= REGISTRATION ================= */

    public PublicKeyCredentialCreationOptions startRegistration(
            UUID tenantId,
            UUID userId,
            String username,
            String deviceId,
            String origin
    ) {

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

        registrationChallengeService.store(tenantId, deviceId, userId, options);

        return options;
    }

    public void finishRegistration(
            UUID tenantId,
            String deviceId,
            PublicKeyCredential<AuthenticatorAttestationResponse, ClientRegistrationExtensionOutputs> credential,
            String origin
    ) {

        RelyingParty rp = relyingPartyFactory.forOrigin(origin);

        var ctx = registrationChallengeService.getFull(tenantId, deviceId);

        UUID userId = ctx.userId();

        deviceSecurityService.enforceDeviceLimit(tenantId, userId);

        RegistrationResult result;

        try {
            result = rp.finishRegistration(
                    FinishRegistrationOptions.builder()
                            .request(ctx.options())
                            .response(credential)
                            .build()
            );
        } catch (Exception e) {
            e.printStackTrace();
            throw new RuntimeException(e.getMessage(), e);
        }

        String credentialId = credential.getId().getBase64Url();

        System.out.println("STORED credentialId: " + credentialId);

        biometricRepository.findByCredentialId(credentialId)
                .ifPresent(b -> { throw new SecurityException("Already registered"); });

        biometricRepository.save(
                UserBiometric.builder()
                        .userId(userId)
                        .credentialId(credentialId)
                        .publicKey(result.getPublicKeyCose().getBase64())
                        .signCount(result.getSignatureCount())
                        .deviceId(deviceId)
                        .build()
        );
    }
}