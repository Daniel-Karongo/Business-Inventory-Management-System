package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.dto;

import lombok.Data;

import java.util.UUID;

@Data
public class BarcodeScanRequest {
    private String barcode;
    private UUID branchId; // optional but recommended for POS
}