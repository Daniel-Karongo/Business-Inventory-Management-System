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
    private String productName;
    private UUID productVariantId;
    private String productVariantName;
    private UUID branchId;
    private String branchName;

    private String type;
    private Long quantityDelta;
    private BigDecimal unitCost;

    private String reference;
    private UUID supplierId;

    private String note;
    private LocalDateTime timestamp;
    private String performedBy;
}