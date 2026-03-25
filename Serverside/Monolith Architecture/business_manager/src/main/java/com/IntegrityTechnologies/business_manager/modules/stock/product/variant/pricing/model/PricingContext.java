package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.model;

import lombok.Builder;
import lombok.Data;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.UUID;

@Data
@Builder
public class PricingContext {

    private UUID tenantId;
    private UUID branchId;

    private UUID productVariantId;
    private UUID packagingId;

    private Long quantity;
    private BigDecimal cost;

    private UUID customerId;
    private UUID customerGroupId;
    private PricingPolicy policy;

    private LocalDateTime pricingTime;
}