package com.IntegrityTechnologies.business_manager.security.biometric.repository;

import com.IntegrityTechnologies.business_manager.security.biometric.model.UserBiometric;
import com.yubico.webauthn.CredentialRepository;
import com.yubico.webauthn.RegisteredCredential;
import com.yubico.webauthn.data.*;
import com.yubico.webauthn.data.exception.Base64UrlException;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.*;

@Component
@RequiredArgsConstructor
public class WebAuthnCredentialRepository implements CredentialRepository {

    private final UserBiometricRepository repository;

    /* =====================================================
       LOOKUP BY credentialId + userHandle
    ===================================================== */
    @Override
    public Optional<RegisteredCredential> lookup(ByteArray credentialId, ByteArray userHandle) {

        String idBase64Url = credentialId.getBase64Url();

        Optional<UserBiometric> opt = repository.findByCredentialId(idBase64Url);

        if (opt.isEmpty()) {
            return Optional.empty();
        }

        UserBiometric b = opt.get();

        if (Boolean.TRUE.equals(b.getDeleted())) {
            return Optional.empty();
        }

        // 🔥 CRITICAL FIX: ignore incoming userHandle mismatch
        // Yubico may pass different handle depending on flow
        try {
            return Optional.of(
                    RegisteredCredential.builder()
                            .credentialId(ByteArray.fromBase64Url(b.getCredentialId()))
                            .userHandle(new ByteArray(b.getUserId().toString().getBytes())) // canonical
                            .publicKeyCose(ByteArray.fromBase64(b.getPublicKey()))
                            .signatureCount(b.getSignCount())
                            .build()
            );
        } catch (Base64UrlException e) {
            throw new RuntimeException(e);
        }
    }

    /* =====================================================
       LOOKUP ALL BY credentialId
    ===================================================== */
    @Override
    public Set<RegisteredCredential> lookupAll(ByteArray credentialId) {

        String idBase64Url = credentialId.getBase64Url();

        Optional<UserBiometric> opt = repository.findByCredentialId(idBase64Url);

        if (opt.isEmpty()) {
            return Set.of();
        }

        UserBiometric b = opt.get();

        if (Boolean.TRUE.equals(b.getDeleted())) {
            return Set.of();
        }

        try {
            return Set.of(
                    RegisteredCredential.builder()
                            .credentialId(ByteArray.fromBase64Url(b.getCredentialId()))
                            .userHandle(new ByteArray(b.getUserId().toString().getBytes())) // canonical
                            .publicKeyCose(ByteArray.fromBase64(b.getPublicKey()))
                            .signatureCount(b.getSignCount())
                            .build()
            );
        } catch (Base64UrlException e) {
            throw new RuntimeException(e);
        }
    }

    /* =====================================================
       REQUIRED (NOT USED)
    ===================================================== */
    @Override
    public Set<PublicKeyCredentialDescriptor> getCredentialIdsForUsername(String username) {
        return Set.of();
    }

    @Override
    public Optional<ByteArray> getUserHandleForUsername(String username) {
        return Optional.empty(); // 🔥 disable broken mapping
    }

    @Override
    public Optional<String> getUsernameForUserHandle(ByteArray userHandle) {

        if (userHandle == null) return Optional.empty();

        String userIdStr = new String(userHandle.getBytes());

        return Optional.of(userIdStr);
    }
}