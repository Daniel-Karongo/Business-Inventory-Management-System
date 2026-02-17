package com.IntegrityTechnologies.business_manager.modules.stock.product.parent.dto;

import lombok.Data;

import java.util.List;
import java.util.Map;

@Data
public class ProductBulkFileMetadataDTO {

    private String id; // frontend file id

    private String fileName;

    private List<Integer> assignedRowIndexes;

    /**
     * rowIndex -> list of variant classifications
     */
    private Map<Integer, List<String>> rowVariantMap;

    private Boolean assignToEntity;

    private String description;
}