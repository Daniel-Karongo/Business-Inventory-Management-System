package com.IntegrityTechnologies.business_manager.modules.supplier.model;

import com.IntegrityTechnologies.business_manager.modules.supplier.dto.SupplierCreateDTO;
import lombok.Data;

import java.util.List;

@Data
public class SupplierBulkWithFilesDTO {
    private List<SupplierCreateDTO> suppliers;
}
