package com.IntegrityTechnologies.business_manager.modules.stock.onboarding.dto;

import lombok.Builder;
import lombok.Data;

import java.math.BigDecimal;
import java.util.UUID;

@Data
@Builder
public class StockOnboardingResponse {

    private UUID productId;
    private UUID variantId;
    private UUID branchId;

    private long totalUnitsReceived;
    private BigDecimal totalCost;

    private String message;
}