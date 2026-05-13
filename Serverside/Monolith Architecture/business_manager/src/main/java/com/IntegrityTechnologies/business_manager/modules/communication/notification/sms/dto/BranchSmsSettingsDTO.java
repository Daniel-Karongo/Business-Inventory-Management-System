package com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.dto;

import lombok.Data;

@Data
public class BranchSmsSettingsDTO {

    private Boolean enabled;

    private String provider;

    private String username;

    private String apiKey;

    private String senderId;

    private String defaultCountryCode;

    private Boolean sandbox;

    private Boolean active;
}