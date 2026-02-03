package com.IntegrityTechnologies.business_manager.modules.stock.inventory.dto;

import lombok.Data;

import java.math.BigDecimal;
import java.util.List;
import java.util.UUID;

@Data
public class InventoryReceiveBulkRow {

    private UUID productId;

    /** If present → existing variant */
    private UUID productVariantId;

    /** If variantId is null → new variant */
    private String classification;
    private String newVariantSku;

    private UUID branchId;

    private BigDecimal sellingPrice;

    private String reference;
    private String note;

    private List<SupplierUnit> suppliers;
}