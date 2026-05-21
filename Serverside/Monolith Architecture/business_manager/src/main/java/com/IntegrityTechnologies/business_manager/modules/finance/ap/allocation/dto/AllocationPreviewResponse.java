package com.IntegrityTechnologies.business_manager.modules.finance.ap.allocation.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;
import java.util.List;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class AllocationPreviewResponse {

    private BigDecimal requestedAmount;

    private BigDecimal totalAllocated;

    private BigDecimal remainingUnallocated;

    private List<AllocationPreviewItemDto> allocations;
}