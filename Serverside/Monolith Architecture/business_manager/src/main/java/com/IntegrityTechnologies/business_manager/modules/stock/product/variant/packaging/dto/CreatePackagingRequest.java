package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.packaging.dto;

import lombok.Data;

import java.util.UUID;

@Data
public class CreatePackagingRequest {
    private UUID branchId;
    private UUID variantId;
    private String name;
    private Long unitsPerPackaging;
}