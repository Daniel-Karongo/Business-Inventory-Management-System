package com.IntegrityTechnologies.business_manager.modules.inventory.dto;

import lombok.Data;

import java.util.UUID;

@Data
public class ReceiveStockRequest {
    private UUID productId;
    private Long quantity;
    private String reference; // PO number
    private String location; // optional
    private String note;
}