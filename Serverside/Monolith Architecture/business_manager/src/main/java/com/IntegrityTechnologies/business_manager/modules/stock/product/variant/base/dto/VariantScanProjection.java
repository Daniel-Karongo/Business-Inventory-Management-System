package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.dto;

import java.math.BigDecimal;
import java.util.UUID;

public interface VariantScanProjection {

    UUID getProductId();
    String getProductName();

    UUID getVariantId();
    String getClassification();
    String getSku();
    String getBarcode();

    UUID getBranchId();
    Long getQuantityOnHand();
}