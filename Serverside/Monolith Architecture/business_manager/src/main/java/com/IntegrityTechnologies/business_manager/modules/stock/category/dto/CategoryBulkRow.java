package com.IntegrityTechnologies.business_manager.modules.stock.category.dto;

import lombok.Data;

import java.util.Set;

@Data
public class CategoryBulkRow {

    private String name;
    private String description;

    /** Hybrid parent reference (by name) */
    private String parentName;

    /** Optional hybrid supplier references */
    private Set<String> supplierNames;
}