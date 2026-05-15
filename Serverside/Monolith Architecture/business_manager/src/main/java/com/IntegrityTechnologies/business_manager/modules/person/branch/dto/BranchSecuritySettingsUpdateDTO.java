package com.IntegrityTechnologies.business_manager.modules.person.branch.dto;

import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import lombok.Data;

@Data
public class BranchSecuritySettingsUpdateDTO {

    @Min(-90)
    @Max(90)
    private Double latitude;

    @Min(-180)
    @Max(180)
    private Double longitude;

    @Min(1)
    @Max(100000)
    private Integer radiusMeters;

    private Boolean enforceGeofence;

    private Boolean enforceDevice;
}