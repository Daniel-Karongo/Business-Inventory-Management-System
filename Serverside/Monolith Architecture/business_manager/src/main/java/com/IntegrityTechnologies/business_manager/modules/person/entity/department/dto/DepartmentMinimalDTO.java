package com.IntegrityTechnologies.business_manager.modules.person.entity.department.dto;

import com.IntegrityTechnologies.business_manager.modules.person.entity.department.model.Department;
import lombok.Data;

import java.util.UUID;

@Data
public class DepartmentMinimalDTO {
    private UUID id;
    private String name;

    public static DepartmentMinimalDTO from(Department d) {
        DepartmentMinimalDTO dto = new DepartmentMinimalDTO();
        dto.setId(d.getId());
        dto.setName(d.getName());
        return dto;
    }
}
