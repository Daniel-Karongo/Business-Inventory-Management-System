package com.IntegrityTechnologies.business_manager.modules.stock.product.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.PositiveOrZero;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.web.multipart.MultipartFile;

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

    @NotNull(message = "The category is required.")
    private Long categoryId;

    private List<UUID> supplierIds;

    @Schema(type = "array")
    private List<MultipartFile> images;
}