package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.packaging.dto;

import lombok.Data;

import java.util.UUID;

@Data
public class UpdatePackagingRequest {
    private UUID branchId;
    private String name;
    private Long unitsPerPackaging;
}