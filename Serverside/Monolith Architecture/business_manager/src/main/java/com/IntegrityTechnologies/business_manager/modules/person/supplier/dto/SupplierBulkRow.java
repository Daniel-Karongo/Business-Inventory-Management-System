package com.IntegrityTechnologies.business_manager.modules.person.supplier.dto;

import lombok.Data;

import java.util.Set;
import java.util.UUID;

@Data
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