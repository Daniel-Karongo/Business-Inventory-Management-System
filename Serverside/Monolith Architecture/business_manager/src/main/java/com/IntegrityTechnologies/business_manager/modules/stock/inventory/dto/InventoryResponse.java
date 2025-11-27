package com.IntegrityTechnologies.business_manager.modules.stock.inventory.dto;

import lombok.Builder;
import lombok.Data;

import java.util.UUID;

@Data
@Builder
public class InventoryResponse {
    private UUID productId;
    private Long quantityOnHand;
    private Long quantityReserved;
    private String location;
    private String lastUpdatedAt;
}