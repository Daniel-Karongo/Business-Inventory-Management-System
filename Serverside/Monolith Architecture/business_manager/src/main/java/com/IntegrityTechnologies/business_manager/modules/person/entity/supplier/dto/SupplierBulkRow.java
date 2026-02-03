package com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.dto;

import lombok.Data;

import java.util.Set;

@Data
public class SupplierBulkRow {

    private String name;

    /** multi-value supported */
    private Set<String> email;
    private Set<String> phoneNumber;

    private String address;
    private String region;

    /** Hybrid refs */
    private Set<String> categoryNames;
}