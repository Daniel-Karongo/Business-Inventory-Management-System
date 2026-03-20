package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.repository;

import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.model.PricingContext;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.model.PricingResult;

public interface PricingRule {

    boolean applies(PricingContext ctx);

    PricingResult apply(PricingContext ctx, PricingResult current);

    int priority();
}