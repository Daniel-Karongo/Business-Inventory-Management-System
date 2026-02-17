package com.IntegrityTechnologies.business_manager.modules.stock.product.parent.dto;

import lombok.Data;

import java.util.List;

@Data
public class ProductBulkFrontendRowDTO {

    private String name;
    private String description;
    private String categoryName;
    private List<String> supplierNames;
    private List<String> variants;
    private Double minimumPercentageProfit;

}