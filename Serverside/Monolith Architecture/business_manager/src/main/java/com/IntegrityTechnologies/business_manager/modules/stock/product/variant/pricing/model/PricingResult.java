package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.model;

import lombok.Data;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

@Data
public class PricingResult {

    private BigDecimal basePrice;
    private BigDecimal finalPrice;

    private UUID resolvedPriceId;

    private List<PricingAdjustment> adjustments = new ArrayList<>();
}