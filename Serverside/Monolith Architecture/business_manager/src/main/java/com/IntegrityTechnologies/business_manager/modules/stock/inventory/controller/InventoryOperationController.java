package com.IntegrityTechnologies.business_manager.modules.stock.inventory.controller;

import com.IntegrityTechnologies.business_manager.config.response.ApiResponse;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.dto.*;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.service.InventoryService;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/stock/operations")
@RequiredArgsConstructor
public class InventoryOperationController {

    private final InventoryService inventoryService;

    @PostMapping("/receive")
    public ApiResponse receive(@RequestBody ReceiveStockRequest req) {
        return inventoryService.receiveStock(req);
    }

    @PostMapping("/transfer")
    public ApiResponse transfer(@RequestBody TransferStockRequest req) {
        return inventoryService.transferStock(req);
    }

    @PostMapping("/adjust")
    public ApiResponse adjust(@RequestBody AdjustStockRequest req) {
        return inventoryService.adjustStockVariant(req);
    }

    @PostMapping("/consume")
    public ApiResponse consume(@RequestBody DecrementStockRequest req) {
        inventoryService.decrementVariantStock(
                req.getProductVariantId(),
                req.getBranchId(),
                req.getQuantity(),
                req.getReference()
        );
        return new ApiResponse("success", "Stock consumed");
    }
}