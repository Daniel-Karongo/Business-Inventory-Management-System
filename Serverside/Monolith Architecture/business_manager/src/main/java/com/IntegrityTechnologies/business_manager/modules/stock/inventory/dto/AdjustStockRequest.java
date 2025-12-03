package com.IntegrityTechnologies.business_manager.modules.stock.inventory.dto;

import lombok.Data;

import java.util.UUID;

@Data
public class AdjustStockRequest {
    private UUID productVariantId; // changed from productId
    private UUID branchId;
    private long quantityDelta;
    private String reason;
    private String reference;
}