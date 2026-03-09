package com.IntegrityTechnologies.business_manager.security.auth.service;

import org.springframework.stereotype.Service;

import java.time.Instant;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

@Service
public class TokenBlacklistService {

    private final Map<String, Long> blacklist =
            new ConcurrentHashMap<>();

    public void blacklistToken(String token) {

        // expire at midnight
        long expiry = Instant.now().getEpochSecond() + 86400;

        blacklist.put(token, expiry);
    }

    public boolean isTokenBlacklisted(String token) {

        Long expiry = blacklist.get(token);

        if (expiry == null) {
            return false;
        }

        if (expiry < Instant.now().getEpochSecond()) {
            blacklist.remove(token);
            return false;
        }

        return true;
    }
}