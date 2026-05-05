package com.IntegrityTechnologies.business_manager.modules.stock.inventory.controller;

import com.IntegrityTechnologies.business_manager.config.bulk.BulkRequest;
import com.IntegrityTechnologies.business_manager.config.response.ApiResponse;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.dto.InventoryReceiveBulkRow;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.service.InventoryBulkService;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/stock/bulk")
@RequiredArgsConstructor
public class InventoryBulkController {

    private final InventoryBulkService bulkService;

    @PostMapping("/receive")
    public ApiResponse receive(@RequestBody BulkRequest<InventoryReceiveBulkRow> request) {
        return new ApiResponse(
                "success",
                "Bulk receive processed",
                bulkService.bulkReceive(request)
        );
    }
}