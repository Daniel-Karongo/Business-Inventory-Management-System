package com.IntegrityTechnologies.business_manager.security.auth.util;

import jakarta.servlet.http.HttpServletRequest;

import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.util.HexFormat;

public final class DeviceFingerprintUtil {

    private DeviceFingerprintUtil() {}

    public static String generate(HttpServletRequest request) {

        try {

            String ip = extractClientIp(request);
            String userAgent = request.getHeader("User-Agent");

            if (userAgent == null) {
                userAgent = "unknown";
            }

            String raw = ip + "|" + userAgent;

            MessageDigest digest = MessageDigest.getInstance("SHA-256");

            byte[] hash = digest.digest(raw.getBytes(StandardCharsets.UTF_8));

            return HexFormat.of().formatHex(hash);

        } catch (Exception e) {

            throw new RuntimeException("Failed to generate device fingerprint", e);

        }
    }

    private static String extractClientIp(HttpServletRequest request) {

        String xfHeader = request.getHeader("X-Forwarded-For");

        if (xfHeader == null || xfHeader.isBlank()) {
            return request.getRemoteAddr();
        }

        return xfHeader.split(",")[0];
    }
}