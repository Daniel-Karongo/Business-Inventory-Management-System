package com.IntegrityTechnologies.business_manager.modules.stock.product.parent.dto;

import com.IntegrityTechnologies.business_manager.common.bulk.BulkOptions;
import lombok.Data;

import java.util.ArrayList;
import java.util.List;

@Data
public class ProductBulkFrontendRequestDTO {

    private List<ProductBulkFrontendRowDTO> products = new ArrayList<>();

    private List<ProductBulkFileMetadataDTO> fileMetadata = new ArrayList<>();

    private BulkOptions options;
}