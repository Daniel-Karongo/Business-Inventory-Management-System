package com.IntegrityTechnologies.business_manager.modules.person.supplier.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.Data;

import java.util.Set;
import java.util.UUID;

@Data
@JsonIgnoreProperties(ignoreUnknown = true)
public class SupplierBulkRow {
    private UUID branchId;
    private String name;

    /** multi-value supported */
    private Set<String> email;
    private Set<String> phoneNumber;

    private String address;
    private String region;

    /** Hybrid refs */
    private Set<String> categoryNames;
}