package com.IntegrityTechnologies.business_manager.modules.inventory.dto;

import lombok.Data;

import java.util.UUID;

@Data
public class AdjustStockRequest {
    private UUID productId;
    private Long quantityDelta; // positive or negative
    private String reason;
    private String location;
}