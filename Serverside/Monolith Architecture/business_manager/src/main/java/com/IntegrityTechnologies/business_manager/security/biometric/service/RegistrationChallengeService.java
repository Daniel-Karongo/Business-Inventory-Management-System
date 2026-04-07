package com.IntegrityTechnologies.business_manager.security.biometric.service;

import com.yubico.webauthn.data.PublicKeyCredentialCreationOptions;
import org.springframework.stereotype.Service;

import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;

@Service
public class RegistrationChallengeService {

    private final Map<String, Timed> store = new ConcurrentHashMap<>();
    private static final long TTL = 300_000;

    record Timed(
            PublicKeyCredentialCreationOptions options,
            UUID userId,
            long ts
    ) {}

    private String key(UUID tenantId, String fingerprint) {
        return tenantId + "|" + fingerprint;
    }

    public void store(UUID tenantId, String fingerprint, UUID userId,
                      PublicKeyCredentialCreationOptions options) {

        store.put(
                key(tenantId, fingerprint),
                new Timed(options, userId, System.currentTimeMillis())
        );
    }

    public Timed getFull(UUID tenantId, String fingerprint) {
        Timed t = store.remove(key(tenantId, fingerprint));

        if (t == null || System.currentTimeMillis() - t.ts > TTL) {
            throw new SecurityException("Registration challenge expired");
        }

        return t;
    }
}