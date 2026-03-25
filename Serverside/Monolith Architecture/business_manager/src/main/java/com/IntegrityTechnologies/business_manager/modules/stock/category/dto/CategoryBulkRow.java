package com.IntegrityTechnologies.business_manager.modules.stock.category.dto;

import lombok.Data;

import java.math.BigDecimal;
import java.util.Set;
import java.util.UUID;

@Data
public class CategoryBulkRow {

    private String name;
    private String description;
    private UUID branchId;
    private Double minimumPercentageProfit;
    private BigDecimal minimumProfit;

    /** Hybrid parent reference (by name) */
    private String parentName;

    /** Optional hybrid supplier references */
    private Set<String> supplierNames;
}