package com.IntegrityTechnologies.business_manager.modules.person.branch.dto;

import lombok.Data;

import java.time.LocalTime;

@Data
public class BranchAttendanceSettingsDTO {

    private LocalTime rollcallStartTime;

    private Integer rollcallGraceMinutes;

    private LocalTime logoutTime;
}