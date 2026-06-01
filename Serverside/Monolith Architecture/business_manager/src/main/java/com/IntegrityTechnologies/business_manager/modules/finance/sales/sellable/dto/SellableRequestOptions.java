package com.IntegrityTechnologies.business_manager.modules.finance.sales.sellable.dto;

import java.util.UUID;

public interface SellableRequestOptions {

    UUID getBranchId();

    UUID getCustomerId();

    UUID getCustomerGroupId();

    boolean isIncludePricing();

    boolean isIncludeBatches();

    boolean isIncludeAllocation();
}