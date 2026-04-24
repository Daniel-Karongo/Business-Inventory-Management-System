package com.IntegrityTechnologies.business_manager.security.device.dto;

import lombok.Builder;
import lombok.Data;

import java.time.LocalDateTime;
import java.util.UUID;

@Data
@Builder
public class DeviceAttemptDTO {

    private UUID attemptedUserId;

    private String attemptedUsername;

    private String reason;

    private LocalDateTime attemptedAt;
}