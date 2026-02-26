package com.IntegrityTechnologies.business_manager.modules.stock.inventory.dto;

import lombok.Data;

import java.util.List;
import java.util.UUID;

@Data
public class PreviewAllocationRequest {
    private UUID variantId;
    private UUID branchId;
    private long quantity;
    private List<UUID> selectedBatchIds;
}