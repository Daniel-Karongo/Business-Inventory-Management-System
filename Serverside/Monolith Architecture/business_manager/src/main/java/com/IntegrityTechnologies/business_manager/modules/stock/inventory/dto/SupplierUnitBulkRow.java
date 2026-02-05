package com.IntegrityTechnologies.business_manager.modules.stock.inventory.dto;

import lombok.Data;

import java.math.BigDecimal;

@Data
public class SupplierUnitBulkRow {

    private String supplierName;
    private Long unitsSupplied;
    private BigDecimal unitCost;
}