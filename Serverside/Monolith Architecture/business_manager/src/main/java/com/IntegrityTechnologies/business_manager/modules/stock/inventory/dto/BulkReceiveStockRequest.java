package com.IntegrityTechnologies.business_manager.modules.stock.inventory.dto;

import lombok.Data;

import java.util.List;

@Data
public class BulkReceiveStockRequest {
    private List<ReceiveStockRequest> items;
}