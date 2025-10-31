package com.IntegrityTechnologies.business_manager.modules.category.dto;

import lombok.*;
import jakarta.validation.constraints.NotBlank;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class CategoryDTO {

    private Long id;

    @NotBlank(message = "Category name is required")
    private String name;
}