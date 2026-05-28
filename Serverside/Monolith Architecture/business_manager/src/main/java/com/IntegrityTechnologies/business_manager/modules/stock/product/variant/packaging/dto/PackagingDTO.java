package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.packaging.dto;

import lombok.Builder;
import lombok.Data;

import java.util.UUID;

@Data
@Builder
public class PackagingDTO {

    private UUID packagingId;

    private UUID variantId;

    private String name;

    private Long unitsPerPackaging;

    private Boolean isBaseUnit;
}