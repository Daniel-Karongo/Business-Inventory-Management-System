package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.dto;

import lombok.Data;

import java.util.UUID;

@Data
public class BarcodeScanRequest {
    private UUID branchId;
    private String barcode;
}