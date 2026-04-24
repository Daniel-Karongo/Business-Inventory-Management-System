package com.IntegrityTechnologies.business_manager.modules.platform.identity.dto;

import lombok.Data;

import java.util.Set;

@Data
public class PlatformUserCreateRequest {
    private String username;
    private String password;
    private String role;

    private Boolean mustChangePassword;

    private Set<String> emailAddresses;
    private Set<String> phoneNumbers;
    private String idNumber;
}