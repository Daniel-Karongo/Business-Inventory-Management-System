package com.IntegrityTechnologies.business_manager.modules.stock.inventory.dto;

import lombok.Data;

import java.util.UUID;

@Data
public class AdjustStockRequest {
    private UUID productId;
    private UUID branchId;
    private Long quantityDelta;
    private String reason;
}