package com.IntegrityTechnologies.business_manager.modules.platform.tenant.dto;

import lombok.Builder;
import lombok.Data;

import java.time.LocalDateTime;
import java.util.UUID;

@Data
@Builder
public class TenantDTO {

    private UUID id;

    private String code;

    private String name;

    private String status;

    private boolean platformTenant;

    private LocalDateTime createdAt;

}