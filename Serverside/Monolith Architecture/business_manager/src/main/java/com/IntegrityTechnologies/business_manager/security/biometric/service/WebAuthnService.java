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

    public AssertionRequest startAssertion(String deviceId) {

        AssertionRequest request = relyingParty.startAssertion(
                StartAssertionOptions.builder().build()
        );

        challengeService.store(deviceId, request);

        return request;
    }

    public AssertionResult finishAssertion(
            String deviceId,
            PublicKeyCredential<AuthenticatorAssertionResponse, ClientAssertionExtensionOutputs> credential
    ) {

        AssertionRequest request = challengeService.get(deviceId);

        if (request == null) {
            throw new SecurityException("Challenge expired");
        }

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