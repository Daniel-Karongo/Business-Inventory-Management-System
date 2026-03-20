package com.IntegrityTechnologies.business_manager.modules.finance.sales.sellable.dto;

import lombok.Builder;
import lombok.Data;

import java.math.BigDecimal;
import java.util.List;
import java.util.Map;

@Data
@Builder
public class AllocationPreviewDTO {
    private BigDecimal totalCost;
    private List<Map<String, Object>> allocations;
}