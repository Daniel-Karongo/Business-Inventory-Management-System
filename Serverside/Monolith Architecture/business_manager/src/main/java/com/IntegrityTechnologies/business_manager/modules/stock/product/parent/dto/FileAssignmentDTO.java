package com.IntegrityTechnologies.business_manager.modules.stock.product.parent.dto;

import lombok.Data;
import java.util.List;

@Data
public class FileAssignmentDTO {
    private String fileName;
    private Boolean assignToProduct;
    private List<String> variantClassifications;
}