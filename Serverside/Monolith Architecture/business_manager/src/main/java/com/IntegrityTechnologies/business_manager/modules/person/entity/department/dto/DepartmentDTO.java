package com.IntegrityTechnologies.business_manager.modules.person.entity.department.dto;

import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.dto.BranchMinimalDTO;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.dto.MinimalUserDTO;
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
    private Set<UUID> headIds;

    @JsonProperty(access = JsonProperty.Access.READ_ONLY)
    private Set<MinimalUserDTO> heads;

    @JsonProperty(access = JsonProperty.Access.WRITE_ONLY)
    private Set<UUID> memberIds;

    @JsonProperty(access = JsonProperty.Access.READ_ONLY)
    private Set<MinimalUserDTO> members;

    @JsonProperty(access = JsonProperty.Access.WRITE_ONLY)
    private Set<UUID> branchIds;

    @JsonProperty(access = JsonProperty.Access.READ_ONLY)
    private Set<BranchMinimalDTO> branches;

    @JsonProperty(access = JsonProperty.Access.READ_ONLY)
    private boolean deleted = false;
}