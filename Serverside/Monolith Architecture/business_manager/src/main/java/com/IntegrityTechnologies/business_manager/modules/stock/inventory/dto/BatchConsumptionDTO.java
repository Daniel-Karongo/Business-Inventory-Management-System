package com.IntegrityTechnologies.business_manager.modules.stock.inventory.dto;

import lombok.Builder;
import lombok.Data;

import java.math.BigDecimal;
import java.util.UUID;

@Data
@Builder
public class BatchConsumptionDTO {

    private UUID batchId;
    private UUID saleId;
    private UUID productVariantId;

    private Long quantity;
    private BigDecimal unitCost;

    private BigDecimal totalCost;
}