package com.IntegrityTechnologies.business_manager.security.biometric.service;

public class WebAuthnRequestContext {

    private static final ThreadLocal<String> deviceIdHolder = new ThreadLocal<>();

    public static void setDeviceId(String deviceId) {
        deviceIdHolder.set(deviceId);
    }

    public static String getDeviceId() {
        return deviceIdHolder.get();
    }

    public static void clear() {
        deviceIdHolder.remove();
    }
}