package com.IntegrityTechnologies.business_manager.modules.product.dto;

import com.IntegrityTechnologies.business_manager.modules.product.model.ProductImage;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.PositiveOrZero;
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
public class ProductCreateDTO {

    @NotNull(message = "Product name is required.")
    private String name;

    private String description;

    private String barcode;

    @NotNull(message = "Selling price is required.")
    private BigDecimal price;

    @NotNull(message = "Buying price is required.")
    private BigDecimal buyingPrice;

    @NotNull(message = "The stock quantity is required.")
    @PositiveOrZero(message = "Stock quantity cannot be negative.")
    private Integer stockQuantity;

    @NotNull(message = "The category is required.")
    private Long categoryId;

    private List<UUID> supplierIds;

    @Schema(type = "array")
    private List<ProductImage> images;
}