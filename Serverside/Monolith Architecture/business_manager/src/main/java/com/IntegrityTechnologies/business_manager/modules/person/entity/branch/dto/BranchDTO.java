package com.IntegrityTechnologies.business_manager.modules.person.entity.branch.dto;

import com.IntegrityTechnologies.business_manager.modules.person.entity.user.dto.MinimalUserDTO;
import com.IntegrityTechnologies.business_manager.modules.person.entity.department.dto.DepartmentMinimalDTO;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Builder;
import lombok.Data;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Set;
import java.util.UUID;

@Data
@Builder
public class BranchDTO {
    @JsonProperty(access = JsonProperty.Access.READ_ONLY)
    private UUID id;
    private String branchCode;
    private String name;
    private String location;
    private String phone;
    private String email;
    private LocalDateTime createdAt;
    @JsonProperty(access = JsonProperty.Access.WRITE_ONLY)
    private List<UUID> userIds;
    @JsonProperty(access = JsonProperty.Access.READ_ONLY)
    private Set<MinimalUserDTO> users;
    @JsonProperty(access = JsonProperty.Access.WRITE_ONLY)
    private List<UUID> departmentIds;
    @JsonProperty(access = JsonProperty.Access.READ_ONLY)
    private Set<DepartmentMinimalDTO> departments;
    @JsonProperty(access = JsonProperty.Access.READ_ONLY)
    private Boolean deleted;
}
