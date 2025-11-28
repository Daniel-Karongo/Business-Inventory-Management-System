package com.IntegrityTechnologies.business_manager.modules.stock.inventory.dto;

import lombok.Data;
import java.util.UUID;

@Data
public class ReleaseStockRequest {
    private UUID productId;
    private UUID branchId;
    private Integer quantity;
    private String reference;
}