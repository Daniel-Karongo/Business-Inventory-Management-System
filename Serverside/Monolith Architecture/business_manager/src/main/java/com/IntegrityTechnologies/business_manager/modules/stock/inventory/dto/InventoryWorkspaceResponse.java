package com.IntegrityTechnologies.business_manager.modules.stock.inventory.dto;

import lombok.Builder;
import lombok.Data;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.UUID;

@Data
@Builder
public class InventoryWorkspaceResponse {

    /* ==========================
       PRODUCT
    ========================== */

    private UUID productId;
    private String productName;
    private String productSku;

    /* ==========================
       VARIANT
    ========================== */

    private UUID productVariantId;
    private String productVariantSku;
    private String productClassification;

    /* ==========================
       BRANCH
    ========================== */

    private UUID branchId;

    /* ==========================
       STOCK
    ========================== */

    private Long quantityOnHand;
    private Long quantityReserved;
    private Long quantityAvailable;

    /* ==========================
       COSTING
    ========================== */

    private BigDecimal averageCost;
    private BigDecimal inventoryValue;

    /* ==========================
       SELLING
    ========================== */

    private BigDecimal sellingPrice;
    private BigDecimal marginAmount;
    private BigDecimal marginPercent;

    /* ==========================
       BATCHES
    ========================== */

    private Integer batchCount;
    private BigDecimal totalRemainingBatchValue;

    private LocalDateTime oldestBatchDate;

    /* ==========================
       MOVEMENT
    ========================== */

    private LocalDateTime lastReceiptDate;
    private Long lastReceiptQuantity;

    private LocalDateTime lastSaleDate;
    private Long lastSaleQuantity;

    private LocalDateTime lastTransferInDate;
    private Long lastTransferInQuantity;

    private LocalDateTime lastTransferOutDate;
    private Long lastTransferOutQuantity;

    /* ==========================
       AUDIT
    ========================== */

    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;
}