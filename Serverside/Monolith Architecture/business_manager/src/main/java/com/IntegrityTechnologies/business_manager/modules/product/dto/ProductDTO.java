package com.IntegrityTechnologies.business_manager.modules.product.dto;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class ProductDTO {
    private UUID id;
    private String name;
    private String description;
    private String sku;
    private String barcode;
    private String barcodeImagePath;
    private BigDecimal price;
    private BigDecimal buyingPrice;
    private Integer stockQuantity;
    private List<String> imageUrls;
    private Long categoryId;
    private String categoryName;
    private List<UUID> supplierIds;
    private UUID lastSupplierId;
    private String lastSupplierName;
    private Boolean deleted;
    private LocalDateTime deletedAt;
    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;
}