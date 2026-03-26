package com.IntegrityTechnologies.business_manager.modules.finance.sales.sellable.dto;

import lombok.Builder;
import lombok.Data;

import java.math.BigDecimal;
import java.util.List;

@Data
@Builder
public class AllocationResult {

    private List<AllocationDetail> allocations;
    private BigDecimal totalCost;
}
