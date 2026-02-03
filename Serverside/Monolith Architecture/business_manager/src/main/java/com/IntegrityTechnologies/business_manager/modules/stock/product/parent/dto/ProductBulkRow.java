package com.IntegrityTechnologies.business_manager.modules.stock.product.parent.dto;

import lombok.Data;

import java.util.List;
import java.util.Set;

@Data
public class ProductBulkRow {

    private String name;
    private String description;
    private String barcode;

    /** Hybrid refs */
    private String categoryName;
    private Set<String> supplierNames;

    /** Variant support */
    private List<String> variants;   // e.g. ["Small", "Medium", "Large"]

    private Double minimumPercentageProfit;
}