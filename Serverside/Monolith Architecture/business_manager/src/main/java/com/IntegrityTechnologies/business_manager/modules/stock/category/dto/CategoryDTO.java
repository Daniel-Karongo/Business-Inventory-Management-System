package com.IntegrityTechnologies.business_manager.modules.stock.category.dto;

import com.IntegrityTechnologies.business_manager.modules.person.supplier.dto.SupplierMinimalDTO;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;
import jakarta.validation.constraints.NotBlank;

import java.math.BigDecimal;
import java.util.List;
import java.util.Set;
import java.util.UUID;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class CategoryDTO {
    @JsonProperty(access = JsonProperty.Access.READ_ONLY)
    private Long id;

    @NotBlank(message = "Category name is required")
    private String name;

    private String description;
    private UUID branchId;

    private Double minimumPercentageProfit;
    private BigDecimal minimumProfit;

    @JsonProperty(access = JsonProperty.Access.READ_ONLY)
    private boolean deleted;

    private Long parentId; // parent category
    @JsonProperty(access = JsonProperty.Access.READ_ONLY)
    private String parentName;

    @JsonProperty(access = JsonProperty.Access.READ_ONLY)
    private List<CategoryDTO> subcategories; // optional nested DTOs

    @JsonProperty(access = JsonProperty.Access.WRITE_ONLY)
    private Set<UUID> supplierIds; // optional nested DTOs

    @JsonProperty(access = JsonProperty.Access.READ_ONLY)
    private List<SupplierMinimalDTO> suppliers;
}

