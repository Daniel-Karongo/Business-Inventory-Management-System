package com.IntegrityTechnologies.business_manager.modules.finance.sales.dto;

import lombok.Data;

import java.util.UUID;

@Data
public class BatchSelectionDto {
    private UUID batchId;
    private Long quantity;
}