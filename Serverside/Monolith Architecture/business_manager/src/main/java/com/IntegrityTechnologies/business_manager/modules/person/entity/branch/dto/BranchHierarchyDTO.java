package com.IntegrityTechnologies.business_manager.modules.person.entity.branch.dto;

import com.IntegrityTechnologies.business_manager.modules.person.entity.department.dto.DepartmentPositionDTO;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;
import java.util.UUID;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class BranchHierarchyDTO {
    private UUID branchId;
    private String branchName;
    private List<DepartmentPositionDTO> departments;
}
