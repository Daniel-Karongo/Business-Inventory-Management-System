package com.IntegrityTechnologies.business_manager.modules.platform.identity.dto;

import lombok.Data;

@Data
public class PlatformUserCreateRequest {

    private String username;

    private String password;

    private String role;

}