package com.IntegrityTechnologies.business_manager.modules.finance.sales.sellable.dto;

import lombok.Data;

import java.util.UUID;

@Data
public class SellableProductRequest {

    private UUID branchId;

    private String search;

    private UUID customerId;
    private UUID customerGroupId;

    private Long quantity;

    // flags
    private boolean includePricing = true;
    private boolean includeBatches = false;
    private boolean includeAllocation = false;

    // 🔥 NEW
    private Integer page = 0;
    private Integer size = 20;
}