package com.IntegrityTechnologies.business_manager.modules.person.department.dto;

import com.IntegrityTechnologies.business_manager.modules.person.branch.dto.BranchMinimalDTO;
import com.IntegrityTechnologies.business_manager.modules.person.user.dto.MinimalUserDTO;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

import java.time.LocalTime;
import java.util.Set;
import java.util.UUID;

@Data
public class DepartmentDTO {

    @JsonProperty(access = JsonProperty.Access.READ_ONLY)
    private UUID id;

    private String name;
    private String description;

    private LocalTime rollcallStartTime;
    private Integer gracePeriodMinutes;

    @JsonProperty(access = JsonProperty.Access.WRITE_ONLY)
    private UUID branchId;

    @JsonProperty(access = JsonProperty.Access.READ_ONLY)
    private BranchMinimalDTO branch;

    @JsonProperty(access = JsonProperty.Access.WRITE_ONLY)
    private Set<UUID> headIds;

    @JsonProperty(access = JsonProperty.Access.WRITE_ONLY)
    private Set<UUID> memberIds;

    @JsonProperty(access = JsonProperty.Access.READ_ONLY)
    private Set<MinimalUserDTO> heads;

    @JsonProperty(access = JsonProperty.Access.READ_ONLY)
    private Set<MinimalUserDTO> members;

    @JsonProperty(access = JsonProperty.Access.READ_ONLY)
    private boolean deleted;
}