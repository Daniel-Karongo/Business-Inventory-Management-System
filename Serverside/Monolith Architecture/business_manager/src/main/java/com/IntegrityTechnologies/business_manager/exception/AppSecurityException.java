package com.IntegrityTechnologies.business_manager.exception;

import com.IntegrityTechnologies.business_manager.security.model.SecurityErrorCode;

public class AppSecurityException extends RuntimeException {

    private final SecurityErrorCode code;

    public AppSecurityException(SecurityErrorCode code, String message) {
        super(message);
        this.code = code;
    }

    public SecurityErrorCode getCode() {
        return code;
    }
}