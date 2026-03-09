package com.IntegrityTechnologies.business_manager.modules.platform.tenant.dto;

import lombok.Data;

@Data
public class TenantCreateRequest {

    private String name;

    private String code;

    private String adminUsername;

    private String adminPassword;

}