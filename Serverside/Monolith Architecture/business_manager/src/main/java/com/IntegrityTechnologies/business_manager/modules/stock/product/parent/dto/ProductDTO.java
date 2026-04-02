package com.IntegrityTechnologies.business_manager.modules.stock.product.parent.dto;

import com.IntegrityTechnologies.business_manager.modules.person.supplier.dto.SupplierMinimalDTO;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.dto.ProductVariantDTO;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

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
    private UUID branchId;

    private Double minimumPercentageProfit;
    private BigDecimal minimumProfit;

    private List<ProductVariantDTO> variants; // <-- NEW
    private List<String> imageUrls;
    private Long categoryId;
    private String categoryName;

    private List<SupplierMinimalDTO> suppliers;
    private Boolean deleted;
    private LocalDateTime deletedAt;
    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;
}