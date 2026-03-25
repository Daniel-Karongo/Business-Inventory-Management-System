package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.dto;

import lombok.Data;
import java.math.BigDecimal;

@Data
public class ProductVariantUpdateDTO {
    private String classification;
    private Double minimumPercentageProfit;
    private BigDecimal minimumProfit;
    private String sku;
}