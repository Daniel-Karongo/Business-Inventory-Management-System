package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.repository;

import java.util.UUID;

public interface VariantThumbnailProjection {

    UUID getVariantId();

    String getThumbnailFileName();
}