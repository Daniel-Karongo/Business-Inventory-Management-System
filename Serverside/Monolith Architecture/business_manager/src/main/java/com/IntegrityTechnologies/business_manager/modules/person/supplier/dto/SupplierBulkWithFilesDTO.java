package com.IntegrityTechnologies.business_manager.modules.person.supplier.dto;

import lombok.Data;

import java.util.List;

@Data
public class SupplierBulkWithFilesDTO {
    private List<SupplierCreateDTO> suppliers;
}
