package com.IntegrityTechnologies.business_manager.security.model;

public enum SecurityErrorCode {

    /* ================= AUTH ================= */
    INVALID_CREDENTIALS,
    USER_NOT_FOUND,
    ACCOUNT_DISABLED,
    PASSWORD_CHANGE_REQUIRED,
    SESSION_EXPIRED,
    PASSWORD_RESET_INVALID,

    /* ================= BIOMETRIC ================= */
    BIOMETRIC_INVALID_PAYLOAD,
    BIOMETRIC_VERIFICATION_FAILED,
    BIOMETRIC_CREDENTIAL_NOT_FOUND,
    BIOMETRIC_DEVICE_MISMATCH,
    BIOMETRIC_TENANT_MISMATCH,
    BIOMETRIC_USER_MISMATCH,
    BIOMETRIC_CHALLENGE_EXPIRED,
    ACCOUNT_DELETED,
    /* ================= DEVICE ================= */
    DEVICE_ID_REQUIRED,
    DEVICE_NOT_REGISTERED,
    DEVICE_NOT_APPROVED,
    DEVICE_PENDING_APPROVAL,
    DEVICE_LIMIT_REACHED,
    DEVICE_NOT_FOUND,

    /* ================= LOCATION ================= */
    LOCATION_REQUIRED,
    LOCATION_OUTSIDE_BOUNDARY,
    LOCATION_ACCURACY_LOW,
    LOCATION_NOT_CONFIGURED,

    /* ================= ACCESS ================= */
    USER_NOT_IN_BRANCH,
    BRANCH_NOT_FOUND,

    /* ================= SYSTEM ================= */
    INVALID_REQUEST,
    UNKNOWN
}