package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.dto;

import java.math.BigDecimal;
import java.util.UUID;

public interface VariantScanProjection {

    UUID getProductId();
    String getProductName();

    UUID getVariantId();
    String getClassification();
    String getSku();
    String getBarcode();

    BigDecimal getSellingPrice();

    UUID getBranchId();
    Long getQuantityOnHand();
}