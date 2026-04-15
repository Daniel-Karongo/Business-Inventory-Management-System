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

    /* ================= ASSERTION ================= */

    public AssertionRequest startAssertion(UUID tenantId, String deviceId, String origin) {

        RelyingParty rp = relyingPartyFactory.forOrigin(origin);

        AssertionRequest request = rp.startAssertion(
                StartAssertionOptions.builder().build()
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

            var userHandle = result.getUserHandle();
            if (userHandle == null) throw new SecurityException("Missing user handle");

            UUID userId = UUID.fromString(new String(userHandle.getBytes()));

            String credentialId = result.getCredentialId().getBase64Url();

            var biometric = biometricRepository.findByCredentialId(credentialId)
                    .orElseThrow(() -> new SecurityException("Credential not recognized"));

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
            throw new SecurityException("WebAuthn verification failed", e);
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
                                .extensions(RegistrationExtensionInputs.builder().credProps().build())
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