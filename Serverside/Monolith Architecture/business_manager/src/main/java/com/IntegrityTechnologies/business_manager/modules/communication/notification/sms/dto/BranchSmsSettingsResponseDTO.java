package com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.dto;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class BranchSmsSettingsResponseDTO {

    private Boolean enabled;

    private String provider;

    private String username;

    private String apiKey;

    private String senderId;

    private String defaultCountryCode;

    private Boolean sandbox;

    private Boolean active;
}