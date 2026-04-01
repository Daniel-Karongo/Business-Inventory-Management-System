package com.IntegrityTechnologies.business_manager.security.biometric.repository;

import com.yubico.webauthn.CredentialRepository;
import com.yubico.webauthn.RegisteredCredential;
import com.yubico.webauthn.data.*;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.*;

@Component
@RequiredArgsConstructor
public class WebAuthnCredentialRepository implements CredentialRepository {

    private final UserBiometricRepository repository;

    @Override
    public Set<PublicKeyCredentialDescriptor> getCredentialIdsForUsername(String username) {
        return Set.of(); // not used (you use userId)
    }

    @Override
    public Optional<ByteArray> getUserHandleForUsername(String username) {
        return Optional.empty();
    }

    @Override
    public Optional<String> getUsernameForUserHandle(ByteArray userHandle) {
        return Optional.empty();
    }

    @Override
    public Optional<RegisteredCredential> lookup(ByteArray credentialId, ByteArray userHandle) {

        return repository.findByCredentialId(credentialId.getBase64Url())
                .map(b -> RegisteredCredential.builder()
                        .credentialId(credentialId)
                        .userHandle(new ByteArray(b.getUserId().toString().getBytes()))
                        .publicKeyCose(ByteArray.fromBase64(b.getPublicKey()))
                        .signatureCount(b.getSignCount())
                        .build());
    }

    @Override
    public Set<RegisteredCredential> lookupAll(ByteArray credentialId) {

        return repository.findByCredentialId(credentialId.getBase64Url())
                .map(b -> Set.of(
                        RegisteredCredential.builder()
                                .credentialId(credentialId)
                                .userHandle(new ByteArray(b.getUserId().toString().getBytes()))
                                .publicKeyCose(ByteArray.fromBase64(b.getPublicKey()))
                                .signatureCount(b.getSignCount())
                                .build()
                ))
                .orElse(Set.of());
    }
}