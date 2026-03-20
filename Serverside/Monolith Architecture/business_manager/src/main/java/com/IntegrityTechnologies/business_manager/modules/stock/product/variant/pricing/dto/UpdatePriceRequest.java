package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.dto;

import lombok.Data;

import java.math.BigDecimal;

@Data
public class UpdatePriceRequest {
    private BigDecimal price;
}
