package com.IntegrityTechnologies.business_manager.modules.product.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;
import java.util.List;
import java.util.UUID;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class ProductUpdateDTO {
    private String name;
    private String description;
    private String barcode;
    private BigDecimal price;
    private BigDecimal buyingPrice;
    private Integer stockQuantity;
    private Long categoryId;
    private List<UUID> supplierIds;
}