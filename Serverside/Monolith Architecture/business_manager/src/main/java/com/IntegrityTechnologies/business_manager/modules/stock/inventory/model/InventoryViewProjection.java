package com.IntegrityTechnologies.business_manager.modules.stock.inventory.model;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.UUID;

public interface InventoryViewProjection {

    UUID getProductId();

    String getProductName();

    String getProductSku();

    UUID getProductVariantId();

    String getProductClassification();

    String getProductVariantSku();

    UUID getBranchId();

    Long getQuantityOnHand();

    BigDecimal getAverageCost();

    LocalDateTime getLastUpdatedAt();
}