package com.IntegrityTechnologies.business_manager.modules.person.entity.branch.dto;

import lombok.Data;

@Data
public class BranchBulkRow {

    private String branchCode;
    private String name;
    private String location;
    private String phone;
    private String email;
}