package com.IntegrityTechnologies.business_manager.modules.stock.product.parent.dto;

import com.IntegrityTechnologies.business_manager.common.bulk.BulkOptions;
import lombok.Data;

import java.util.ArrayList;
import java.util.List;

@Data
public class ProductBulkFullCreateDTO {

    /**
     * Each entry represents one product with:
     *  - ProductCreateDTO
     *  - Variant DTOs
     *  - File assignments
     */
    private List<ProductFullCreateDTO> products = new ArrayList<>();

    /**
     * Dry run / skip duplicate etc.
     */
    private BulkOptions options;
}