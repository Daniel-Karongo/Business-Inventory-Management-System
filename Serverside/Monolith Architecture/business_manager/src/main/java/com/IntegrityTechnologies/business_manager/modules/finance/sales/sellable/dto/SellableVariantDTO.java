package com.IntegrityTechnologies.business_manager.modules.finance.sales.sellable.dto;

import lombok.Builder;
import lombok.Data;

import java.math.BigDecimal;
import java.util.List;
import java.util.Map;
import java.util.UUID;

@Data
@Builder
public class SellableVariantDTO {

    private UUID productId;
    private String productName;
    private String productSku;

    private UUID variantId;
    private String variantSku;
    private String classification;

    private Long quantityOnHand;
    private Long quantityReserved;
    private Long quantityAvailable;

    private List<PackagingDTO> packagings;

    // ✅ NEW
    private Map<UUID, PricingPreviewDTO> pricingByPackaging;

    private List<BatchPreviewDTO> batches;
    private AllocationPreviewDTO allocation;

    private List<WarningDTO> warnings;
}