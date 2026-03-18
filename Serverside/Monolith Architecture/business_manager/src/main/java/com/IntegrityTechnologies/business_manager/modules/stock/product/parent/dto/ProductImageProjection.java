package com.IntegrityTechnologies.business_manager.modules.stock.product.parent.dto;

import java.util.UUID;

public interface ProductImageProjection {
    UUID getProductId();
    String getFilePath();
}