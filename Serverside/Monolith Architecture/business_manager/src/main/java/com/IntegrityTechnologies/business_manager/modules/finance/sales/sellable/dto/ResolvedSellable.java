package com.IntegrityTechnologies.business_manager.modules.finance.sales.sellable.dto;

import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.model.PricingAdjustment;
import lombok.Builder;
import lombok.Data;

import java.math.BigDecimal;
import java.util.List;

@Data
@Builder
public class ResolvedSellable {
    private BigDecimal unitPrice;
    private BigDecimal totalPrice;
    private long availableQuantity;
    private long baseUnits;
    private List<PricingAdjustment> adjustments;
}
