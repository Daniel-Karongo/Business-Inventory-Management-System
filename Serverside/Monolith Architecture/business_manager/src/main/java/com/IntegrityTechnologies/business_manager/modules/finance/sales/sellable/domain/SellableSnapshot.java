package com.IntegrityTechnologies.business_manager.modules.finance.sales.sellable.domain;

import com.IntegrityTechnologies.business_manager.modules.finance.sales.sellable.dto.AllocationDetail;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.model.PricingAdjustment;
import lombok.Builder;
import lombok.Data;

import java.math.BigDecimal;
import java.util.List;
import java.util.UUID;

@Data
@Builder
public class SellableSnapshot {

    private UUID productVariantId;
    private UUID packagingId;

    private long quantity;
    private long baseUnits;

    private BigDecimal unitPrice;
    private BigDecimal totalPrice;

    private BigDecimal unitCost;
    private BigDecimal totalCost;

    private BigDecimal netAmount;
    private BigDecimal vatAmount;
    private BigDecimal vatRate;

    private long availableStock;

    private List<AllocationDetail> allocations;

    private List<PricingAdjustment> adjustments;

    private Boolean stockSufficient;
    private List<String> warnings;

    private String pricingJson;
    private String resolutionMode;
}