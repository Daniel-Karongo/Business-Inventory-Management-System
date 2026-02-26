package com.IntegrityTechnologies.business_manager.modules.stock.inventory.dto;

import lombok.Data;

import java.math.BigDecimal;
import java.util.UUID;

@Data
public class AdjustStockRequest {
    private UUID productVariantId; // changed from productId
    private UUID branchId;
    private long quantityDelta;
    private BigDecimal unitCost; // required when quantityDelta > 0
    private String reason;
    private String reference;
}