package com.IntegrityTechnologies.business_manager.modules.stock.product.parent.dto;

import com.IntegrityTechnologies.business_manager.modules.stock.inventory.dto.ProductRestoreOptions;
import lombok.Data;

@Data
public class ProductRestoreRequest {

    private String reason;
    private ProductRestoreOptions restoreOptions;
}
