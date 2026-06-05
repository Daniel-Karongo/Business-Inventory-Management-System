package com.IntegrityTechnologies.business_manager.modules.person.branch.dto;

import lombok.Builder;
import lombok.Data;

import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

@Data
@Builder
public class BranchListItemDTO {

    private UUID id;

    private String branchCode;

    private String name;

    private String location;

    private Boolean deleted;

    private String phone;

    private String email;

    private LocalDateTime createdAt;
}