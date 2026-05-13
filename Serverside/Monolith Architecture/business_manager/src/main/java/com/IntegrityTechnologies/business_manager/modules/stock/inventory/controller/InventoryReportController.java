package com.IntegrityTechnologies.business_manager.modules.stock.inventory.controller;

import com.IntegrityTechnologies.business_manager.config.response.ApiResponse;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.service.InventoryService;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Pageable;
import org.springframework.web.bind.annotation.*;

import java.util.UUID;

@RestController
@RequestMapping("/api/stock/reports")
@RequiredArgsConstructor
public class InventoryReportController {

    private final InventoryService inventoryService;

    @GetMapping("/low-stock")
    public ApiResponse lowStock(@RequestParam(defaultValue = "10") Long threshold,
                                Pageable pageable) {
        return inventoryService.getLowStock(threshold, pageable);
    }

    @GetMapping("/out-of-stock")
    public ApiResponse outOfStock(Pageable pageable) {
        return inventoryService.getOutOfStock(pageable);
    }

    @GetMapping("/audit/{productId}")
    public ApiResponse audit(
            @PathVariable UUID productId,
            @RequestParam UUID branchId
    ) {
        return new ApiResponse("success", "Audit",
                inventoryService.getAuditTrail(branchId, productId));
    }
}