package com.IntegrityTechnologies.business_manager.modules.finance.sales.sellable.dto;

import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.model.PricingAdjustment;
import lombok.Builder;
import lombok.Data;

import java.math.BigDecimal;
import java.util.List;

@Data
@Builder
public class PricingPreviewDTO {
    private BigDecimal unitPrice;
    private BigDecimal totalPrice;
    private List<PricingAdjustment> adjustments;
}