package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.events;

import lombok.Builder;
import lombok.Data;

import java.util.List;
import java.util.UUID;

@Builder
@Data
public class VariantBarcodePdfRequestedEvent {

    private UUID tenantId;
    private UUID branchId;

    private List<UUID> variantIds;
    private UUID productId;

    private String outputPath; // ✅ NEW
}