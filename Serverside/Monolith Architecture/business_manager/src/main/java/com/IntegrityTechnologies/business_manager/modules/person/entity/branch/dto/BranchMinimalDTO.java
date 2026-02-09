package com.IntegrityTechnologies.business_manager.modules.person.entity.branch.dto;

import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.model.Branch;
import com.IntegrityTechnologies.business_manager.modules.person.entity.department.dto.DepartmentDTO;
import com.IntegrityTechnologies.business_manager.modules.person.entity.department.model.Department;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.dto.MinimalUserDTO;
import lombok.*;

import java.util.UUID;
import java.util.stream.Collectors;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class BranchMinimalDTO {
    private UUID id;
    private String branchCode;
    private String name;

    public static BranchMinimalDTO from(Branch b) {
        BranchMinimalDTO dto = new BranchMinimalDTO();
        dto.setId(b.getId());
        dto.setName(b.getName());
        dto.setBranchCode(b.getBranchCode());
        return dto;
    }
}