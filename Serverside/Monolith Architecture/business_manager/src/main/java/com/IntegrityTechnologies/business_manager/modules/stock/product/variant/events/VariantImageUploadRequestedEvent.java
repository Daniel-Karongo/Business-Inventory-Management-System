package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.events;

import lombok.*;

import java.util.UUID;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class VariantImageUploadRequestedEvent {

    private UUID variantId;
    private UUID tenantId;
    private UUID branchId;

    private String fileName;
    private String tempFilePath;
}