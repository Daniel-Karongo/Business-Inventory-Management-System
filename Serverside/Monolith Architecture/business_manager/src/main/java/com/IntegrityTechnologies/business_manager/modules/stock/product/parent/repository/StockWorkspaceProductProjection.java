package com.IntegrityTechnologies.business_manager.modules.stock.product.parent.repository;

import java.time.LocalDateTime;
import java.util.UUID;

public interface StockWorkspaceProductProjection {

    UUID getId();

    String getName();

    String getSku();

    Boolean getDeleted();

    LocalDateTime getCreatedAt();

    LocalDateTime getUpdatedAt();

    Long getCategoryId();

    String getCategoryName();

    String getThumbnailFileName();

    String getPrimaryImageFileName();

    long getVariantCount();
}