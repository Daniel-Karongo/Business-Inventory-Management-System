package com.IntegrityTechnologies.business_manager.modules.finance.sales.sellable.dto;

import lombok.Data;

import java.util.List;
import java.util.UUID;

@Data
public class SellableResolveRequest {

    private UUID productVariantId;
    private UUID packagingId;

    private Long quantity; // sell units (not base)

    private UUID branchId;

    private UUID customerId;
    private UUID customerGroupId;

    private List<UUID> batchIds; // optional
}