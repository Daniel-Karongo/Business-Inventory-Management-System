package com.IntegrityTechnologies.business_manager.modules.stock.inventory.dto;

import lombok.Builder;
import lombok.Data;

import java.math.BigDecimal;
import java.util.UUID;

@Data
@Builder
public class InventoryBulkPreviewRow {

    private UUID productId;
    private String productName;

    private UUID productVariantId;
    private String variantClassification;
    private String variantSku;

    private UUID branchId;
    private String branchName;

    private Long unitsReceived;
    private BigDecimal unitCost;
    private BigDecimal sellingPrice;
    private BigDecimal totalCost;
}