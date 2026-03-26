package com.IntegrityTechnologies.business_manager.modules.finance.sales.sellable.dto;

import lombok.Builder;
import lombok.Data;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.UUID;

@Data
@Builder
public class AllocationDetail {

    private UUID batchId;

    private long allocatedQuantity;
    private long availableQuantity;
    private long reservedQuantity;

    private BigDecimal unitCost;
    private BigDecimal totalCost;

    private LocalDateTime receivedAt; // 🔥 future FEFO ready
}