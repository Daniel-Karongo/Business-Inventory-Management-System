package com.IntegrityTechnologies.business_manager.security.biometric.service;

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

    private final RelyingParty relyingParty;
    private final ChallengeService challengeService;
    private final UserBiometricRepository biometricRepository;
    private final RegistrationChallengeService registrationChallengeService;
    private final DeviceSecurityService deviceSecurityService;

    public AssertionRequest startAssertion(UUID tenantId, String deviceId) {

        AssertionRequest request = relyingParty.startAssertion(
                StartAssertionOptions.builder().build()
        );

        challengeService.store(tenantId, deviceId, request);

        return request;
    }

    public AssertionResult finishAssertion(
            UUID tenantId,
            String deviceId,
            PublicKeyCredential<AuthenticatorAssertionResponse, ClientAssertionExtensionOutputs> credential
    ) {

        AssertionRequest request = challengeService.get(tenantId, deviceId);

        try {

            AssertionResult result = relyingParty.finishAssertion(
                    FinishAssertionOptions.builder()
                            .request(request)
                            .response(credential)
                            .build()
            );

            if (!result.isSuccess()) {
                throw new SecurityException("WebAuthn verification failed");
            }

            /* =====================================================
               ✅ ADD THIS BLOCK (CRITICAL)
            ===================================================== */

            var userHandle = result.getUserHandle();

            if (userHandle == null) {
                throw new SecurityException("Missing user handle");
            }

            String userIdFromHandle = new String(userHandle.getBytes());
            UUID userId = UUID.fromString(userIdFromHandle);

            String credentialId = result.getCredentialId().getBase64Url();

            var biometric = biometricRepository
                    .findByCredentialId(credentialId)
                    .orElseThrow(() -> new SecurityException("Credential not recognized"));

            if (!biometric.getDeviceId().equals(deviceId)) {
                throw new SecurityException("Biometric used on wrong device");
            }

            long newCount = result.getSignatureCount();

            if (newCount > biometric.getSignCount()) {
                biometric.setSignCount(newCount);
                biometricRepository.save(biometric);
            }

            UUID expectedUserId = biometric.getUserId();

            if (!expectedUserId.equals(userId)) {
                throw new SecurityException("User mismatch in WebAuthn assertion");
            }

            return result;

        } catch (AssertionFailedException e) {
            throw new SecurityException("WebAuthn verification failed", e);
        }
    }

    public PublicKeyCredentialCreationOptions startRegistration(
            UUID tenantId,
            UUID userId,
            String username,
            String deviceId
    ) {
        UserIdentity user = UserIdentity.builder()
                .name(username)
                .displayName(username)
                .id(new ByteArray(userId.toString().getBytes()))
                .build();

        PublicKeyCredentialCreationOptions options =
                relyingParty.startRegistration(
                        StartRegistrationOptions.builder()
                                .user(user)
                                .build()
                );

        registrationChallengeService.store(
                tenantId,
                deviceId,
                userId,
                options
        );
        return options;
    }

    public void finishRegistration(
            UUID tenantId,
            String deviceId,
            PublicKeyCredential<AuthenticatorAttestationResponse, ClientRegistrationExtensionOutputs> credential
    ) {
        var ctx = registrationChallengeService.getFull(tenantId, deviceId);

        PublicKeyCredentialCreationOptions request = ctx.options();
        UUID userId = ctx.userId();

        deviceSecurityService.enforceDeviceLimit(tenantId, userId);

        RegistrationResult result;

        try {
            result = relyingParty.finishRegistration(
                    FinishRegistrationOptions.builder()
                            .request(request)
                            .response(credential)
                            .build()
            );
        } catch (Exception e) {
            throw new SecurityException("Registration failed", e);
        }

        String credentialId = credential.getId().getBase64Url();

        biometricRepository.findByCredentialId(credentialId)
                .ifPresent(b -> {
                    throw new SecurityException("Credential already registered");
                });

        UserBiometric biometric = UserBiometric.builder()
                .userId(userId)
                .credentialId(credentialId)
                .publicKey(result.getPublicKeyCose().getBase64())
                .signCount(result.getSignatureCount())
                .deviceId(deviceId)
                .build();

        biometricRepository.save(biometric);
    }

    public UUID resolveUserId(String credentialId) {

        return biometricRepository
                .findByCredentialId(credentialId)
                .map(b -> b.getUserId())
                .orElseThrow(() -> new SecurityException("Credential not recognized"));
    }

    public UserBiometric getBiometricByCredentialId(String credentialId) {
        return biometricRepository
                .findByCredentialId(credentialId)
                .orElseThrow(() -> new SecurityException("Credential not recognized"));
    }
}