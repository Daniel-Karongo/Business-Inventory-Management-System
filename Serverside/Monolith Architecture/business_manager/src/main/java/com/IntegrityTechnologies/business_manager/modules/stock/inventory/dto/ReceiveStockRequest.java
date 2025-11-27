package com.IntegrityTechnologies.business_manager.modules.stock.inventory.dto;

import lombok.Data;

import java.util.List;
import java.util.UUID;

@Data
public class ReceiveStockRequest {
    private UUID productId;
    private Long quantity;
    private List<UUID> supplierIds;
    private String reference; // PO number
    private String location; // optional
    private String note;
}