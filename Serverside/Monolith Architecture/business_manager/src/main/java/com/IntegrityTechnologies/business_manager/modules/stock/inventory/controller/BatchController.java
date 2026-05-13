package com.IntegrityTechnologies.business_manager.modules.stock.inventory.controller;

import com.IntegrityTechnologies.business_manager.config.response.ApiResponse;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.service.InventoryService;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

import java.util.UUID;

@RestController
@RequestMapping("/api/stock/batches")
@RequiredArgsConstructor
public class BatchController {

    private final InventoryService inventoryService;

    @GetMapping("/variant/{variantId}/branch/{branchId}")
    public ApiResponse batches(@PathVariable UUID variantId,
                               @PathVariable UUID branchId) {
        return new ApiResponse("success", "Batches",
                inventoryService.getBatchesForVariantBranch(variantId, branchId));
    }

    @GetMapping("/{batchId}/consumptions")
    public ApiResponse consumptions(
            @PathVariable UUID batchId,
            @RequestParam UUID branchId
    ) {
        return new ApiResponse("success", "Consumptions",
                inventoryService.getBatchConsumptions(branchId, batchId));
    }

    @GetMapping("/suggest")
    public ApiResponse suggest(
            @RequestParam UUID variantId,
            @RequestParam UUID branchId,
            @RequestParam long quantity
    ) {
        return new ApiResponse("success", "Suggested",
                inventoryService.suggestBatches(variantId, branchId, quantity));
    }
}