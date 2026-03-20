package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.model;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class PricingPolicy {

    private boolean enforceMinimumPrice;   // 🔒 strict mode
    private boolean allowManualOverride;   // 👤 user override
}