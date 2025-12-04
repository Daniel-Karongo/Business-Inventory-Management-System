package com.IntegrityTechnologies.business_manager.modules.stock.product.parent.dto;

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

    @NotNull(message = "The category is required.")
    private Long categoryId;

    private Double minimumPercentageProfit;

    private List<UUID> supplierIds;

    private List<String> variants; // <-- NEW: list of classifications to auto-create variants

    @Schema(type = "array")
    private List<MultipartFile> images;
}