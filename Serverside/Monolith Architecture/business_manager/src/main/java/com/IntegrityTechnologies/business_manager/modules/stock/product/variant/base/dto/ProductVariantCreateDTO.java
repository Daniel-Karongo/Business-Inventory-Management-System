package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.dto;

import lombok.Data;

import java.math.BigDecimal;
import java.util.UUID;

@Data
public class ProductVariantCreateDTO {
    private UUID productId;
    private String classification;
    private String barcode;
    private Double minimumPercentageProfit;
    private BigDecimal minimumProfit;
    private String sku;
    private Boolean autoCreateBasePackaging = true;
}