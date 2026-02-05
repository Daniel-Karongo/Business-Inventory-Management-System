package com.IntegrityTechnologies.business_manager.modules.stock.inventory.dto;

import lombok.Builder;
import lombok.Data;

import java.math.BigDecimal;
import java.util.List;

@Data
@Builder
public class InventoryBulkPreviewResult {

    private List<InventoryBulkPreviewRow> rows;
    private long totalUnits;
    private BigDecimal totalCost;
}