package com.IntegrityTechnologies.business_manager.modules.person.function.branch.dto;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Builder;
import lombok.Data;
import lombok.ToString;

import java.util.UUID;

@Data
@Builder
public class BranchDTO {
    @JsonProperty(access = JsonProperty.Access.READ_ONLY)
    private UUID id;
    private String branchCode;
    private String name;
    private String location;
    private String phone;
    private String email;
    @JsonProperty(access = JsonProperty.Access.READ_ONLY)
    private Boolean deleted;
}
