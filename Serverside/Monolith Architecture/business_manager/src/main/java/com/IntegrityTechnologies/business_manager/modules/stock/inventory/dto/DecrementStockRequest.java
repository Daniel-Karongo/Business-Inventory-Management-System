package com.IntegrityTechnologies.business_manager.modules.stock.inventory.dto;

import lombok.Data;
import java.util.UUID;

@Data
public class DecrementStockRequest {
    private UUID productId;
    private UUID BranchId;
    private Integer quantity;
    private String reference;
}