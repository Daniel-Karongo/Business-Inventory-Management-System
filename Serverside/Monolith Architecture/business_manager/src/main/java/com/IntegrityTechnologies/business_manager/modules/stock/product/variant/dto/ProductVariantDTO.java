package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.dto;

import lombok.Builder;
import lombok.Data;

import java.math.BigDecimal;
import java.util.List;
import java.util.UUID;

@Data
@Builder
public class ProductVariantDTO {

    private UUID id;
    private UUID productId;
    private String productName;
    private String classification;

    private BigDecimal minimumSellingPrice;
    private BigDecimal averageBuyingPrice;
    private String sku;
    private String barcode;
    private String barcodeImagePath;
    private List<String> imageUrls;
}