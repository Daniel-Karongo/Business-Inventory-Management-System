package com.IntegrityTechnologies.business_manager.modules.stock.inventory.dto;

import lombok.Data;

import java.math.BigDecimal;
import java.util.UUID;

@Data
public class SupplierUnit {
    private UUID supplierId;
    private Long unitsSupplied;
    private BigDecimal unitCost;
}