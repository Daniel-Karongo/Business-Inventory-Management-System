package com.IntegrityTechnologies.business_manager.modules.stock.inventory.dto;

import lombok.Data;
import java.util.UUID;

@Data
public class ReleaseStockRequest {
    private UUID productVariantId;
    private UUID branchId;
    private int quantity;
    private String reference;
}