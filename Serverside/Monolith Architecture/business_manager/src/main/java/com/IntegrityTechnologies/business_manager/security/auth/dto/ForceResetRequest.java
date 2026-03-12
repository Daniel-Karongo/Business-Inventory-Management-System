package com.IntegrityTechnologies.business_manager.security.auth.dto;

import lombok.Data;

@Data
public class ForceResetRequest {

    private String identifier;
    private String newPassword;

}