package com.IntegrityTechnologies.business_manager.modules.finance.sales.sellable.dto;

import lombok.Data;

import java.util.UUID;

@Data
public class SellableVariantRequest implements SellableRequestOptions {

    private UUID branchId;

    private UUID variantId;

    private UUID customerId;

    private UUID customerGroupId;

    private Long quantity = 1L;

    private boolean includePricing = true;

    private boolean includeBatches = false;

    private boolean includeAllocation = false;
}