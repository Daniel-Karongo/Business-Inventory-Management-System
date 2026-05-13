package com.IntegrityTechnologies.business_manager.modules.person.department.dto;

import lombok.Data;

import java.util.Set;
import java.util.UUID;

@Data
public class DepartmentBulkRow {

    private String name;
    private String description;
    private UUID branchId;
    /** HH:mm (optional) */
    private String rollcallStartTime;
    private Integer gracePeriodMinutes;

    /** Hybrid refs */
    private Set<String> branchCodes;
    private Set<String> headUsernames;
    private Set<String> memberUsernames;
}