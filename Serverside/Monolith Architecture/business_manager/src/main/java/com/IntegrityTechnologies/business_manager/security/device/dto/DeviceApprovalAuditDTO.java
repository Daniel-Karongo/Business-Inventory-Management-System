package com.IntegrityTechnologies.business_manager.security.device.dto;

import lombok.Builder;
import lombok.Data;

import java.time.LocalDateTime;
import java.util.UUID;

@Data
@Builder
public class DeviceApprovalAuditDTO {

    private UUID actedByUserId;

    private String action;

    private String reason;

    private LocalDateTime actedAt;
}