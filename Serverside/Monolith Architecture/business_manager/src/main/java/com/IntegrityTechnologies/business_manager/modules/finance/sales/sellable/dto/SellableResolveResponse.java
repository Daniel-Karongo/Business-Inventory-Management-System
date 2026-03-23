package com.IntegrityTechnologies.business_manager.modules.finance.sales.sellable.dto;

import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.model.PricingAdjustment;
import lombok.Builder;
import lombok.Data;

import java.math.BigDecimal;
import java.util.List;
import java.util.Map;
import java.util.UUID;

@Data
@Builder
public class SellableResolveResponse {

    private UUID productVariantId;
    private UUID packagingId;

    private Long requestedQuantity;
    private Long baseUnits;

    private BigDecimal unitPrice;
    private BigDecimal totalPrice;

    private Long availableStock;

    private BigDecimal totalCost;

    private List<Map<String, Object>> batchAllocations;

    private List<PricingAdjustment> adjustments;
}