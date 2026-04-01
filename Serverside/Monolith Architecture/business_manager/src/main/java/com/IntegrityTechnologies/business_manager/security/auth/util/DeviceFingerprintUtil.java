package com.IntegrityTechnologies.business_manager.security.auth.util;

import jakarta.servlet.http.HttpServletRequest;

import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.util.HexFormat;

public final class DeviceFingerprintUtil {

    private DeviceFingerprintUtil() {}

    public static String generate(HttpServletRequest request, String deviceId) {
        try {
            if (deviceId == null || deviceId.isBlank()) {
                throw new IllegalArgumentException("Missing deviceId");
            }

            String userAgent = request.getHeader("User-Agent");
            String accept = request.getHeader("Accept");
            String ip = request.getRemoteAddr();

            String raw = userAgent + "|" + accept + "|" + deviceId + "|" + ip;

            MessageDigest digest = MessageDigest.getInstance("SHA-256");
            byte[] hash = digest.digest(raw.getBytes(StandardCharsets.UTF_8));

            return HexFormat.of().formatHex(hash);

        } catch (Exception e) {
            throw new RuntimeException("Fingerprint generation failed", e);
        }
    }
}