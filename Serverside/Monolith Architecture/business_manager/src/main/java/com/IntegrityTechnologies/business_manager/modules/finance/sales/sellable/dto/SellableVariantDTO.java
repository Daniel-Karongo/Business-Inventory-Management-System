package com.IntegrityTechnologies.business_manager.modules.finance.sales.sellable.dto;

import lombok.Builder;
import lombok.Data;

import java.math.BigDecimal;
import java.util.List;
import java.util.UUID;

@Data
@Builder
public class SellableVariantDTO {

    // =========================
    // PRODUCT INFO
    // =========================
    private UUID productId;
    private String productName;
    private String productSku;

    // =========================
    // VARIANT INFO
    // =========================
    private UUID variantId;
    private String variantSku;
    private String classification;

    // =========================
    // STOCK
    // =========================
    private Long quantityOnHand;
    private Long quantityReserved;
    private Long quantityAvailable;

    // =========================
    // PACKAGING
    // =========================
    private List<PackagingDTO> packagings;

    // =========================
    // OPTIONAL ENRICHMENTS
    // =========================
    private PricingPreviewDTO pricing;
    private List<BatchPreviewDTO> batches;
    private AllocationPreviewDTO allocation;

    // =========================
    // WARNINGS
    // =========================
    private List<WarningDTO> warnings;
}