package com.IntegrityTechnologies.business_manager.modules.platform.identity.dto;

import lombok.Builder;
import lombok.Data;

import java.time.LocalDateTime;
import java.util.UUID;

@Data
@Builder
public class PlatformUserAuditResponse {

    private UUID id;

    private UUID userId;

    private String username;

    private String action;

    private String reason;

    private String performedBy;

    private LocalDateTime timestamp;

}