package com.IntegrityTechnologies.business_manager.security.biometric.service;

import com.IntegrityTechnologies.business_manager.security.biometric.repository.UserBiometricRepository;
import com.yubico.webauthn.*;
import com.yubico.webauthn.data.AuthenticatorAssertionResponse;
import com.yubico.webauthn.data.ClientAssertionExtensionOutputs;
import com.yubico.webauthn.data.PublicKeyCredential;
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

    public AssertionRequest startAssertion(UUID tenantId, String fingerprint) {

        AssertionRequest request = relyingParty.startAssertion(
                StartAssertionOptions.builder().build()
        );

        challengeService.store(tenantId, fingerprint, request);

        return request;
    }

    public AssertionResult finishAssertion(
            UUID tenantId,
            String fingerprint,
            PublicKeyCredential<AuthenticatorAssertionResponse, ClientAssertionExtensionOutputs> credential
    ) {

        AssertionRequest request = challengeService.get(tenantId, fingerprint);

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

    public UUID resolveUserId(String credentialId) {

        return biometricRepository
                .findByCredentialId(credentialId)
                .map(b -> b.getUserId())
                .orElseThrow(() -> new SecurityException("Credential not recognized"));
    }
}