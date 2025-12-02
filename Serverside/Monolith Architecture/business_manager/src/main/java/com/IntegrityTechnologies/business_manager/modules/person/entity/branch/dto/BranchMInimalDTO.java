package com.IntegrityTechnologies.business_manager.modules.person.entity.branch.dto;

import lombok.Builder;
import lombok.Data;

import java.util.UUID;

@Data
@Builder
public class BranchMInimalDTO {
    private UUID id;
    private String branchCode;
    private String name;
}