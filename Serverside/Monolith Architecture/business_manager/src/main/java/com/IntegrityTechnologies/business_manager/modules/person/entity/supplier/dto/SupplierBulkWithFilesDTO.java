package com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.dto;

import com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.dto.SupplierCreateDTO;
import lombok.Data;

import java.util.List;

@Data
public class SupplierBulkWithFilesDTO {
    private List<SupplierCreateDTO> suppliers;
}
