package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.model;

import lombok.AllArgsConstructor;
import lombok.Data;

import java.math.BigDecimal;

@Data
@AllArgsConstructor
public class PricingAdjustment {

    private String type;
    private String source;
    private BigDecimal amount;
    private String description;
}