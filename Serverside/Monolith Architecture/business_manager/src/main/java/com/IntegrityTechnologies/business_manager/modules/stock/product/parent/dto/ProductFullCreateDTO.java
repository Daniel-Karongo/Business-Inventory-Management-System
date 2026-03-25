package com.IntegrityTechnologies.business_manager.modules.stock.product.parent.dto;

import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.dto.ProductVariantCreateDTO;
import lombok.Data;
import java.util.List;

@Data
public class ProductFullCreateDTO {

    private ProductCreateDTO product;

    private List<ProductVariantCreateDTO> variants;

    private List<FileAssignmentDTO> fileAssignments;
}