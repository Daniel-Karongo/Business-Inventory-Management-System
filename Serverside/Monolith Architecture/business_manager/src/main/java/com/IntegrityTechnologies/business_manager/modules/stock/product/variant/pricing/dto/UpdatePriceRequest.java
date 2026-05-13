package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.dto;

import lombok.Data;

import java.math.BigDecimal;
import java.util.UUID;

@Data
public class UpdatePriceRequest {
    private UUID branchId;
    private BigDecimal price;
}
