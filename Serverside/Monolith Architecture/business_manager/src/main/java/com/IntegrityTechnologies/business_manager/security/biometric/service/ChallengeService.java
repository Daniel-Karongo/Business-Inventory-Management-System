package com.IntegrityTechnologies.business_manager.security.biometric.service;

import com.yubico.webauthn.AssertionRequest;
import org.springframework.stereotype.Service;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

@Service
public class ChallengeService {

    private final Map<String, TimedRequest> store = new ConcurrentHashMap<>();

    private static final long TTL_MS = 120_000;

    record TimedRequest(AssertionRequest request, long timestamp) {}

    public void store(String key, AssertionRequest request) {
        store.put(key, new TimedRequest(request, System.currentTimeMillis()));
    }

    public AssertionRequest get(String key) {

        TimedRequest tr = store.remove(key);

        if (tr == null || System.currentTimeMillis() - tr.timestamp > TTL_MS) {
            throw new SecurityException("Challenge expired");
        }

        return tr.request();
    }
}