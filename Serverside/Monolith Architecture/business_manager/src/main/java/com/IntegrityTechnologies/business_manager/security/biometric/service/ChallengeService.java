package com.IntegrityTechnologies.business_manager.security.biometric.service;

import com.yubico.webauthn.AssertionRequest;
import org.springframework.stereotype.Service;

import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;

@Service
public class ChallengeService {

    private final Map<String, TimedRequest> store = new ConcurrentHashMap<>();

    private static final long TTL_MS = 300_000; // 5 minutes

    record TimedRequest(AssertionRequest request, long timestamp) {}

    private String key(UUID tenantId, String fingerprint) {
        return tenantId + "|" + fingerprint;
    }

    public void store(UUID tenantId, String fingerprint, AssertionRequest request) {
        store.put(key(tenantId, fingerprint),
                new TimedRequest(request, System.currentTimeMillis()));
    }

    public AssertionRequest get(UUID tenantId, String fingerprint) {

        String key = key(tenantId, fingerprint);

        TimedRequest tr = store.remove(key);

        if (tr == null || System.currentTimeMillis() - tr.timestamp > TTL_MS) {
            throw new SecurityException("Challenge expired");
        }

        return tr.request();
    }
}