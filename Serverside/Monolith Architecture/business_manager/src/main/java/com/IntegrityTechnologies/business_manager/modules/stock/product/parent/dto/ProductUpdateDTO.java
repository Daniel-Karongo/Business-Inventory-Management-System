package com.IntegrityTechnologies.business_manager.modules.stock.product.parent.dto;

import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.dto.ProductVariantUpdateDTO;
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

    private String sku;

    private Double minimumPercentageProfit;

    private BigDecimal minimumProfit;

    private Long categoryId;

    private List<UUID> supplierIds;

    private List<ProductVariantUpdateDTO> variants;
}