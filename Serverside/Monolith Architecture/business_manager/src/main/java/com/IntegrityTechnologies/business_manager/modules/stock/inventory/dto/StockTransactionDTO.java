package com.IntegrityTechnologies.business_manager.modules.stock.inventory.dto;

import lombok.Builder;
import lombok.Data;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.UUID;

@Data
@Builder
public class StockTransactionDTO {
    private UUID id;
    private UUID productId;
    private UUID productVariantId;
    private UUID branchId;

    private String type;
    private Long quantityDelta;
    private BigDecimal unitCost;

    private String reference;
    private UUID supplierId;

    private String note;
    private LocalDateTime timestamp;
    private String performedBy;
}