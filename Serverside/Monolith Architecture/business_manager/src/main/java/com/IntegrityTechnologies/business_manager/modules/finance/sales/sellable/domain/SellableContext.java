package com.IntegrityTechnologies.business_manager.modules.finance.sales.sellable.domain;

import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.model.PricingPolicy;
import lombok.Builder;
import lombok.Data;

import java.util.List;
import java.util.UUID;

@Data
@Builder
public class SellableContext {

    private UUID tenantId;
    private UUID branchId;

    private UUID productVariantId;
    private UUID packagingId;

    private long quantity;

    private UUID customerId;
    private UUID customerGroupId;

    private List<UUID> batchIds;

    private PricingPolicy pricingPolicy;

    private ResolutionMode mode;
}