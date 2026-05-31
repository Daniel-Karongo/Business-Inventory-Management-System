package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.dto;

public record VariantImageDTO(
        String fileName,
        String url,
        String thumbnailUrl,
        boolean deleted
) {}