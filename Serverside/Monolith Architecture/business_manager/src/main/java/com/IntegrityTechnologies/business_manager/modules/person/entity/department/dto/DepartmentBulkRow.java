package com.IntegrityTechnologies.business_manager.modules.person.entity.department.dto;

import lombok.Data;

import java.util.Set;

@Data
public class DepartmentBulkRow {

    private String name;
    private String description;

    /** HH:mm (optional) */
    private String rollcallStartTime;
    private Integer gracePeriodMinutes;

    /** Hybrid refs */
    private Set<String> branchCodes;
    private Set<String> headUsernames;
    private Set<String> memberUsernames;
}