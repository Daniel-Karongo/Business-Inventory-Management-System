package com.IntegrityTechnologies.business_manager.modules.person.branch.dto;

import lombok.Builder;
import lombok.Data;

import java.time.LocalDateTime;
import java.util.UUID;

@Data
@Builder
public class BranchAuditDTO {

    private UUID id;

    private UUID branchId;

    private String branchName;

    private String action;

    private String fieldChanged;

    private String oldValue;

    private String newValue;

    private String reason;

    private UUID performedById;

    private String performedByUsername;

    private LocalDateTime timestamp;
}