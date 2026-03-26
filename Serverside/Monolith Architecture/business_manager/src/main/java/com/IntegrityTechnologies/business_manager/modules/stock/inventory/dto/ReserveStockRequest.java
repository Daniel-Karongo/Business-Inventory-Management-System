package com.IntegrityTechnologies.business_manager.modules.stock.inventory.dto;

import com.IntegrityTechnologies.business_manager.modules.finance.sales.base.model.SaleLineBatchSelection;
import lombok.Data;

import java.util.List;
import java.util.UUID;

@Data
public class ReserveStockRequest {
    private UUID productVariantId;
    private UUID packagingId;     // ✅ ADD
    private UUID branchId;
    private Long baseUnits;       // ✅ ADD
    private Long quantity;        // sell units
    private String reference;
    private List<SaleLineBatchSelection> batchSelections;
}