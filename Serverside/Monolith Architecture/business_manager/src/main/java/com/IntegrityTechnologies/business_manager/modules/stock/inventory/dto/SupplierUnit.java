package com.IntegrityTechnologies.business_manager.modules.stock.inventory.dto;

import lombok.Data;

import java.util.UUID;

@Data
public class SupplierUnit {
    private UUID supplierId;
    private Long unitsSupplied;
}