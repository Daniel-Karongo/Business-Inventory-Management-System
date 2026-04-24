package com.IntegrityTechnologies.business_manager.modules.platform.identity.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.Data;

import java.util.Set;

@JsonIgnoreProperties(ignoreUnknown = true)
@Data
public class PlatformUserUpdateRequest {

    private String username;
    private String role;

    private Boolean active;
    private Boolean locked;

    private Boolean mustChangePassword;

    private Set<String> emailAddresses;
    private Set<String> phoneNumbers;

    private String idNumber;

    /* self-service only */
    private String password;
}