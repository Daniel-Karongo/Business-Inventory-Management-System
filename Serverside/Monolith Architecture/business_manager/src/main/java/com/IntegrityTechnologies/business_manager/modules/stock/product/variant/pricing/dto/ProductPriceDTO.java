package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.dto;

import lombok.Builder;
import lombok.Data;

import java.math.BigDecimal;
import java.util.UUID;

@Data
@Builder
public class ProductPriceDTO {

    private UUID id;

    private UUID variantId;

    private UUID packagingId;

    private String packagingName;

    private Long unitsPerPackaging;

    private BigDecimal price;

    private Long minQuantity;

    private Boolean deleted;
}