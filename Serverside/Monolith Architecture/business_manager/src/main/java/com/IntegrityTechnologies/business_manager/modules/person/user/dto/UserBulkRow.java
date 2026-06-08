package com.IntegrityTechnologies.business_manager.modules.person.user.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.Data;

import java.util.List;

@Data
@JsonIgnoreProperties(ignoreUnknown = true)
public class UserBulkRow {

    private String username;
    private String password;           // optional

    private String role;               // e.g. ADMIN, MANAGER

    private List<String> emailAddresses;
    private List<String> phoneNumbers;

    /** Hybrid org placement */
    private String branchCode;
}