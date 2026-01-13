package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.dto;

import lombok.Builder;
import lombok.Data;

import java.math.BigDecimal;
import java.util.UUID;

@Data
@Builder
public class BarcodeScanResponse {

    private UUID productId;
    private String productName;

    private UUID variantId;
    private String classification;
    private String sku;
    private String barcode;

    private BigDecimal sellingPrice;

    private UUID branchId;
    private Long quantityOnHand;
}