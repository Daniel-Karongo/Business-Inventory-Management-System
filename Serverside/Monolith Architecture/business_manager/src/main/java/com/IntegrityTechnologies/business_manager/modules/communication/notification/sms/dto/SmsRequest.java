package com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.dto;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Size;
import lombok.Data;

import java.util.UUID;

@Data
public class SmsRequest {

    @NotBlank
    private String toPhone;

    @NotBlank
    @Size(max = 1000)
    private String message;

    /**
     * Optional override.
     * If null, the tenant's configured senderId is used.
     * Should be validated against tenant settings.
     */
    private String from;

    /**
     * Audit / traceability (username, system, etc.)
     */
    private String createdBy;
}