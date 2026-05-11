package com.IntegrityTechnologies.business_manager.modules.stock.onboarding.dto;

import lombok.Builder;
import lombok.Data;

import java.math.BigDecimal;
import java.util.List;

@Data
@Builder
public class StockOnboardingBulkPreviewResult {

    private List<StockOnboardingBulkPreviewRow> rows;

    private long totalUnits;

    private BigDecimal totalCost;
}