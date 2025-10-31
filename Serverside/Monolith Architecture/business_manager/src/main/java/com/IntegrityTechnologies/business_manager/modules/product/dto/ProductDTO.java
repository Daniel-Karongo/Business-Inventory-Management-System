package com.IntegrityTechnologies.business_manager.modules.product.dto;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonProperty.Access;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import lombok.*;
import org.springframework.web.multipart.MultipartFile;

import java.math.BigDecimal;
import java.util.List;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class ProductDTO {
    private Long id;

    @NotBlank(message = "Product name is required")
    private String name;

    private String description;

    @NotNull(message = "Product price is required")
    private BigDecimal price;

    @NotNull(message = "Category ID is required")
    private Long categoryId;

    @Schema(description = "List of URLs to the stored images (response only)")
    @JsonProperty(access = Access.READ_ONLY)
    private List<String> imageUrls; // ✅ only shown in responses

    @Schema(type = "array", format = "binary", description = "Product image files (upload only)")
    @JsonProperty(access = Access.WRITE_ONLY)
    private List<MultipartFile> imageFiles; // ✅ only used during upload
}