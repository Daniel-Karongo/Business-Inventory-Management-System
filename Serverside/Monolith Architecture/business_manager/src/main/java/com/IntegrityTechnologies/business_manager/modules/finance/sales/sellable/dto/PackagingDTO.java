package com.IntegrityTechnologies.business_manager.modules.finance.sales.sellable.dto;

import lombok.Builder;
import lombok.Data;

import java.util.UUID;

@Data
@Builder
public class PackagingDTO {
    private UUID packagingId;
    private String name;
    private Long unitsPerPackaging;
    private Boolean isBaseUnit;
}