package com.IntegrityTechnologies.business_manager.modules.finance.sales.dto;

import lombok.Data;

import java.math.BigDecimal;
import java.util.List;
import java.util.UUID;

@Data
public class SaleLineDto {
    private UUID productVariantId;   // REQUIRED
    private UUID branchId;
    private Integer quantity;
    private BigDecimal unitPrice; // optional
    private List<BatchSelectionDto> batchSelections;
}
