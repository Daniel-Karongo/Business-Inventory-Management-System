package com.IntegrityTechnologies.business_manager.modules.stock.inventory.dto;

import lombok.Data;

import java.math.BigDecimal;
import java.util.List;
import java.util.UUID;

@Data
public class ReceiveStockRequest {
    private UUID productId;
    private UUID productVariantId;   // optional: if provided, use it
    private String classification;   // optional: if variantId not provided
    private String newVariantSku;    // optional
    private UUID branchId;
    private BigDecimal sellingPrice;
    private List<SupplierUnit> suppliers;
    private String reference;
    private String note;
}