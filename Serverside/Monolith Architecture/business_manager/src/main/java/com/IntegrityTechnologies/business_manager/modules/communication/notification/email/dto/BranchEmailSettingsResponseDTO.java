package com.IntegrityTechnologies.business_manager.modules.communication.notification.email.dto;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class BranchEmailSettingsResponseDTO {

    private Boolean enabled;

    private String host;

    private Integer port;

    private String username;

    private String password;

    private String fromAddress;

    private Boolean authEnabled;

    private Boolean tlsEnabled;

    private Boolean active;
}