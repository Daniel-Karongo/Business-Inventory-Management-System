package com.IntegrityTechnologies.business_manager.modules.product.dto;

import lombok.Data;

import java.util.List;

@Data
public class ProductBulkWithFilesDTO {
    private List<ProductCreateDTO> products;
}
