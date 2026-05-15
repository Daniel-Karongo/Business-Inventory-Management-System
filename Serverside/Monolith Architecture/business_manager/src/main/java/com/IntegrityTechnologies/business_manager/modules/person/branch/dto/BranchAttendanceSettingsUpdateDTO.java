package com.IntegrityTechnologies.business_manager.modules.person.branch.dto;

import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import lombok.Data;

import java.time.LocalTime;

@Data
public class BranchAttendanceSettingsUpdateDTO {

    private LocalTime rollcallStartTime;

    @Min(0)
    @Max(1440)
    private Integer rollcallGraceMinutes;

    private LocalTime logoutTime;
}