package com.IntegrityTechnologies.business_manager.modules.stock.inventory.dto;

import lombok.Builder;
import lombok.Data;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.UUID;

@Data
@Builder
public class InventoryBatchDTO {

    private UUID batchId;
    private UUID productVariantId;
    private UUID branchId;

    private BigDecimal unitCost;
    private BigDecimal unitSellingPrice;

    private Long quantityReceived;
    private Long quantityRemaining;

    private BigDecimal totalRemainingValue;

    private LocalDateTime receivedAt;
}