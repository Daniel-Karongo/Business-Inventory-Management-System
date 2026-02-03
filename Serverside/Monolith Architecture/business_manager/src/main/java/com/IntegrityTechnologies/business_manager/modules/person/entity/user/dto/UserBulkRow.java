package com.IntegrityTechnologies.business_manager.modules.person.entity.user.dto;

import lombok.Data;

import java.util.List;

@Data
public class UserBulkRow {

    private String username;
    private String password;           // optional

    private String role;               // e.g. ADMIN, MANAGER

    private List<String> emailAddresses;
    private List<String> phoneNumbers;

    /** Hybrid org placement */
    private String branchCode;
    private String departmentName;
    private String position;            // head | member
}