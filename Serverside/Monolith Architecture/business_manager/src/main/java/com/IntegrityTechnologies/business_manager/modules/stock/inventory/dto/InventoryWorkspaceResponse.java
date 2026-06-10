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
    private String branchName;

    /* ==========================
       STOCK
    ========================== */

    private Long quantityOnHand;
    private Long quantityReserved;
    private Long quantityAvailable;

    /* ==========================
       COSTING
    ========================== */

    /**
     * Weighted-average inventory cost.
     * Used for valuation/reporting.
     */
    private BigDecimal averageCost;

    /**
     * Cost of the next FIFO unit expected
     * to be consumed if sold now.
     */
    private BigDecimal projectedNextSaleCost;

    private BigDecimal inventoryValue;

    /* ==========================
       SELLING
    ========================== */

    private BigDecimal sellingPrice;

    /**
     * Legacy average-cost margin.
     */
    private BigDecimal marginAmount;
    private BigDecimal marginPercent;

    /**
     * FIFO-based projected margin.
     */
    private BigDecimal projectedMarginAmount;
    private BigDecimal projectedMarginPercent;

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