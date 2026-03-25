package com.IntegrityTechnologies.business_manager.modules.finance.sales.base.dto;

import lombok.Data;

import java.util.List;
import java.util.UUID;

@Data
public class SaleLinePreviewRequest {

    private UUID productVariantId;
    private UUID packagingId;
    private Long quantity;

    private UUID customerId;
    private UUID customerGroupId;

    private UUID branchId;

    private List<BatchSelectionDto> batchSelections;
}