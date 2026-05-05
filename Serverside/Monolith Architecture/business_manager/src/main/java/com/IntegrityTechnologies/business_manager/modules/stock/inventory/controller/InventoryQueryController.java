package com.IntegrityTechnologies.business_manager.modules.stock.inventory.controller;

import com.IntegrityTechnologies.business_manager.config.response.ApiResponse;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.service.InventoryService;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Pageable;
import org.springframework.web.bind.annotation.*;

import java.util.UUID;

@RestController
@RequestMapping("/api/stock/inventory")
@RequiredArgsConstructor
public class InventoryQueryController {

    private final InventoryService inventoryService;

    @GetMapping
    public ApiResponse list(Pageable pageable) {
        return new ApiResponse("success", "Inventory list",
                inventoryService.getAllInventory(pageable));
    }

    @GetMapping("/branch/{branchId}")
    public ApiResponse byBranch(@PathVariable UUID branchId, Pageable pageable) {
        return new ApiResponse("success", "Branch inventory",
                inventoryService.getInventoryByBranch(branchId, pageable));
    }

    @GetMapping("/variant/{variantId}")
    public ApiResponse variantAll(@PathVariable UUID variantId) {
        return inventoryService.getInventoryForVariantBranch(variantId, null);
    }

    @GetMapping("/variant/{variantId}/branch/{branchId}")
    public ApiResponse variantBranch(
            @PathVariable UUID variantId,
            @PathVariable UUID branchId
    ) {
        return inventoryService.getInventoryForVariantBranch(variantId, branchId);
    }

    @GetMapping("/product/{productId}/branches")
    public ApiResponse productAcrossBranches(@PathVariable UUID productId) {
        return new ApiResponse("success", "Product stock across branches",
                inventoryService.getProductStockAcrossBranches(productId));
    }

    @GetMapping("/product/{productId}/branch/{branchId}")
    public ApiResponse productInBranch(
            @PathVariable UUID productId,
            @PathVariable UUID branchId
    ) {
        return new ApiResponse("success", "Product stock in branch",
                inventoryService.getProductStockInBranch(productId, branchId));
    }
}