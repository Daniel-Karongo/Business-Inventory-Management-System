package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.packaging.dto;

import lombok.Data;

@Data
public class UpdatePackagingRequest {
    private String name;
    private Long unitsPerPackaging;
}