package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.events;

import lombok.*;

import java.util.UUID;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class VariantBarcodeRequestedEvent {

    private UUID variantId;
    private UUID tenantId;
    private UUID branchId;
}