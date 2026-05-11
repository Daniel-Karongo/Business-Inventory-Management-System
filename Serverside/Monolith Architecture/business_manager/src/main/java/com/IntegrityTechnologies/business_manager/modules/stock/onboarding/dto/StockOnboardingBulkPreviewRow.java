package com.IntegrityTechnologies.business_manager.modules.stock.onboarding.dto;

import lombok.Builder;
import lombok.Data;

import java.math.BigDecimal;
import java.util.UUID;

@Data
@Builder
public class StockOnboardingBulkPreviewRow {

    private UUID productId;

    private String productName;

    private UUID variantId;

    private String classification;

    private UUID branchId;

    private long totalUnits;

    private BigDecimal totalCost;

    private boolean existingProduct;

    private boolean existingVariant;

    private boolean categoryCreated;

    private int suppliersCreated;

    private int packagingCount;

    private int pricingCount;
}