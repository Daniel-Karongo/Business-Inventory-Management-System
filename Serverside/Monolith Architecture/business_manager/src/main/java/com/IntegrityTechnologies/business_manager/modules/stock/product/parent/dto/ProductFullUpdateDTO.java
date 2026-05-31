package com.IntegrityTechnologies.business_manager.modules.stock.product.parent.dto;

import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.dto.ProductVariantCreateDTO;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.dto.ProductVariantUpdateDTO;
import lombok.Data;

import java.util.List;

@Data
public class ProductFullUpdateDTO {

    private ProductUpdateDTO product;

    private List<ProductVariantUpdateDTO> variants;

    private List<FileAssignmentDTO> fileAssignments;
}