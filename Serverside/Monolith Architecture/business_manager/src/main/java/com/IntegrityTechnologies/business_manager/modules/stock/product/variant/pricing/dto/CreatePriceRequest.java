package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.dto;

import lombok.Data;

import java.math.BigDecimal;
import java.util.UUID;

@Data
public class CreatePriceRequest {
    private UUID variantId;
    private UUID packagingId;
    private BigDecimal price;
    private Long minQuantity;
}