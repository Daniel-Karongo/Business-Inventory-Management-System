package com.IntegrityTechnologies.business_manager.modules.person.entity.department.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.UUID;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class DepartmentAssignmentDTO {
    private UUID branchId;
    private UUID departmentId;
    private String position; // "head" or "member"
}