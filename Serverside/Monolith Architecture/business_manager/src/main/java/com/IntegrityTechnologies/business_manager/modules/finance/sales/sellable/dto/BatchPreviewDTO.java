package com.IntegrityTechnologies.business_manager.modules.finance.sales.sellable.dto;

import lombok.Builder;
import lombok.Data;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.UUID;

@Data
@Builder
public class BatchPreviewDTO {
    private UUID batchId;
    private Long available;
    private BigDecimal unitCost;
    private BigDecimal sellingPrice;
    private LocalDateTime receivedAt;
}