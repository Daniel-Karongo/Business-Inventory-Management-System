package com.IntegrityTechnologies.business_manager.modules.person.branch.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.UUID;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class BranchHierarchyDTO {
    private UUID branchId;
    private String branchName;
}
