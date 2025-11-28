package com.IntegrityTechnologies.business_manager.modules.person.function.department.dto;

import com.IntegrityTechnologies.business_manager.modules.person.function.department.model.Department;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

import java.time.LocalTime;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

@Data
public class DepartmentDTO {
    @JsonProperty(access = JsonProperty.Access.READ_ONLY)
    private UUID id;
    private String name;
    private String description;
    private LocalTime rollcallStartTime;
    private Integer gracePeriodMinutes;
    private Set<DepartmentUserDTO> heads;
    private Set<DepartmentUserDTO> members;
    @JsonProperty(access = JsonProperty.Access.READ_ONLY)
    private boolean deleted = false;

    public static DepartmentDTO from(Department d) {
        DepartmentDTO dto = new DepartmentDTO();
        dto.setId(d.getId());
        dto.setName(d.getName());
        dto.setDescription(d.getDescription());
        dto.setRollcallStartTime(d.getRollcallStartTime());
        dto.setGracePeriodMinutes(d.getGracePeriodMinutes());
        dto.setHeads(d.getHeads().stream().map(u -> new DepartmentUserDTO(u.getId(), u.getUsername())).collect(Collectors.toSet()));
        dto.setMembers(d.getMembers().stream().map(u -> new DepartmentUserDTO(u.getId(), u.getUsername())).collect(Collectors.toSet()));
        return dto;
    }

}