package com.IntegrityTechnologies.business_manager.modules.person.function.auth.service;

import org.springframework.stereotype.Service;

import java.util.Collections;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Handles blacklisting of JWT tokens for logout functionality.
 */
@Service
public class TokenBlacklistService {

    // Thread-safe in-memory blacklist
    private final Set<String> blacklistedTokens =
            Collections.newSetFromMap(new ConcurrentHashMap<>());

    public void blacklistToken(String token) {
        blacklistedTokens.add(token);
    }

    public boolean isTokenBlacklisted(String token) {
        return blacklistedTokens.contains(token);
    }
}