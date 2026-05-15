package com.IntegrityTechnologies.business_manager.modules.person.branch.dto;

import lombok.Builder;
import lombok.Data;

import java.time.LocalDateTime;
import java.util.UUID;

@Data
@Builder
public class BranchListItemDTO {

    private UUID id;

    private String branchCode;

    private String name;

    private String location;

    private Boolean deleted;

    private Boolean enforceGeofence;

    private Boolean enforceDevice;

    private LocalDateTime createdAt;
}