package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.dto;

import lombok.Data;

import java.util.UUID;

@Data
public class ProductVariantCreateDTO {
    private UUID productId;
    private String classification;
    private String sku;
}