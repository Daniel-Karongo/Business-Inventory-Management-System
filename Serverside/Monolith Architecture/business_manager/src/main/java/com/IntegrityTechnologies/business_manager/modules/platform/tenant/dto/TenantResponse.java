package com.IntegrityTechnologies.business_manager.modules.platform.tenant.dto;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.entity.TenantStatus;
import lombok.Builder;
import lombok.Data;

import java.time.LocalDateTime;
import java.util.UUID;

@Data
@Builder
public class TenantResponse {
    private UUID id;
    private String name;
    private String code;
    private TenantStatus status;
    private boolean platformTenant;
    private LocalDateTime createdAt;
}