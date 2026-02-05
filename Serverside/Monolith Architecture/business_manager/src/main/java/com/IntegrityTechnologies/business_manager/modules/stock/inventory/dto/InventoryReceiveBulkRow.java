package com.IntegrityTechnologies.business_manager.modules.stock.inventory.dto;

import lombok.Data;

import java.math.BigDecimal;
import java.util.List;

@Data
public class InventoryReceiveBulkRow {

    private String productName;
    private String classification; // variant
    private String branchCode;

    private BigDecimal sellingPrice;
    private String reference;
    private String note;

    private List<SupplierUnitBulkRow> suppliers;
}