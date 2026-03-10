package com.IntegrityTechnologies.business_manager.modules.platform.identity.dto;

import lombok.Data;

import java.util.List;

@Data
public class PlatformUserUpdateRequest {

    private String username;

    private String role;

    private Boolean active;

    private Boolean locked;

    private List<String> emailAddresses;

    private List<String> phoneNumbers;

    private String idNumber;

}