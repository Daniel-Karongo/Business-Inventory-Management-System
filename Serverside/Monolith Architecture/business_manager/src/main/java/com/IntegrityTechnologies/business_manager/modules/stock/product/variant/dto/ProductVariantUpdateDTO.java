package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.dto;

import lombok.Data;
import java.math.BigDecimal;

@Data
public class ProductVariantUpdateDTO {
    private String classification;
    private BigDecimal minimumSellingPrice;
    private BigDecimal averageBuyingPrice;
    private String sku;
}