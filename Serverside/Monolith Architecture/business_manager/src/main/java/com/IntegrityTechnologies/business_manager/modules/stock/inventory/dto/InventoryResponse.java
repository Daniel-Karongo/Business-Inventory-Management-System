package com.IntegrityTechnologies.business_manager.modules.stock.inventory.dto;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Builder;
import lombok.Data;

import java.util.UUID;

@Data
@Builder
public class InventoryResponse {
    private UUID productId;
    private String productName;
    private String productSKU;
    private UUID productVariantId;
    private String productClassification;
    private String productVariantSKU;
    private UUID branchId;
    private String branchName;
    private Long quantityOnHand;
    private Long quantityReserved;
    private String lastUpdatedAt;
}