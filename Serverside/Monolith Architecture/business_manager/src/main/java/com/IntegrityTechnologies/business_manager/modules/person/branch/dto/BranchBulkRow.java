package com.IntegrityTechnologies.business_manager.modules.person.branch.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.Data;

@Data
@JsonIgnoreProperties(ignoreUnknown = true)
public class BranchBulkRow {

    private String branchCode;
    private String name;
    private String location;
    private String phone;
    private String email;
}