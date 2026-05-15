package com.IntegrityTechnologies.business_manager.modules.person.branch.dto;

import lombok.Data;

@Data
public class BranchSecuritySettingsDTO {

    private Double latitude;

    private Double longitude;

    private Integer radiusMeters;

    private Boolean enforceGeofence;

    private Boolean enforceDevice;
}