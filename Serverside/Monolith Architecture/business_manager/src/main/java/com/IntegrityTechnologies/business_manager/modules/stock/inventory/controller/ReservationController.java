package com.IntegrityTechnologies.business_manager.modules.stock.inventory.controller;

import com.IntegrityTechnologies.business_manager.config.response.ApiResponse;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.dto.*;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.service.InventoryService;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/stock/reservations")
@RequiredArgsConstructor
public class ReservationController {

    private final InventoryService inventoryService;

    @PostMapping("/reserve")
    public ApiResponse reserve(@RequestBody ReserveStockRequest req) {
        inventoryService.reserveStockVariant(
                req.getProductVariantId(),
                req.getPackagingId(),
                req.getBranchId(),
                req.getBaseUnits(),
                req.getQuantity(),
                req.getReference(),
                req.getBatchSelections()
        );
        return new ApiResponse("success", "Reserved");
    }

    @PostMapping("/release")
    public ApiResponse release(@RequestBody ReleaseStockRequest req) {
        inventoryService.releaseReservationVariant(
                req.getBranchId(),
                req.getReference()
        );
        return new ApiResponse("success", "Released");
    }

    @PostMapping("/preview")
    public ApiResponse preview(@RequestBody PreviewAllocationRequest req) {
        return new ApiResponse("success", "Preview",
                inventoryService.previewAllocation(
                        req.getVariantId(),
                        req.getBranchId(),
                        req.getQuantity(),
                        req.getSelectedBatchIds()
                ));
    }
}