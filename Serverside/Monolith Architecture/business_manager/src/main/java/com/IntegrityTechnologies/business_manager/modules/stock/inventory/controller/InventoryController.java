package com.IntegrityTechnologies.business_manager.modules.stock.inventory.controller;

import com.IntegrityTechnologies.business_manager.common.ApiResponse;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.dto.*;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.service.InventoryService;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository.StockTransactionRepository;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;

import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

@Tag(name = "Inventory")
@RestController
@RequestMapping("/api/inventory")
@RequiredArgsConstructor
public class InventoryController {

    private final InventoryService inventoryService;
    private final StockTransactionRepository stockTransactionRepository;

    /** -------------------------
     * EXISTING ENDPOINTS
     * ------------------------- */

    @PreAuthorize("hasAnyRole('SUPERUSER','ADMIN','MANAGER','SUPERVISOR')")
    @PostMapping("/receive")
    public ResponseEntity<ApiResponse> receiveStock(@RequestBody ReceiveStockRequest req) {
        return ResponseEntity.ok(inventoryService.receiveStock(req));
    }

    @PreAuthorize("hasAnyRole('SUPERUSER','ADMIN','MANAGER','SUPERVISOR')")
    @PostMapping("/receive/bulk")
    public ResponseEntity<ApiResponse> bulkReceiveStock(@RequestBody BulkReceiveStockRequest req) {
        List<ReceiveStockRequest> stockRequests = req.getItems();
        List<InventoryResponse> responses = new ArrayList<>();
        for(ReceiveStockRequest stockRequest: stockRequests) {
            responses.add((InventoryResponse) inventoryService.receiveStock(stockRequest).getData());
        }
        return ResponseEntity.ok(new ApiResponse("success", "Inventory Updated successfully", responses));
    }

    @PreAuthorize("hasAnyRole('SUPERUSER','ADMIN','MANAGER','SUPERVISOR')")
    @PostMapping("/adjust")
    public ResponseEntity<ApiResponse> adjustStock(@RequestBody AdjustStockRequest req) {
        return ResponseEntity.ok(inventoryService.adjustStock(req));
    }

    @GetMapping("/product")
    public ResponseEntity<ApiResponse> getInventory(
            @RequestParam UUID productId,
            @RequestParam(required = false) UUID branchtId
    ) {
        return ResponseEntity.ok(inventoryService.getInventoryForBranchProduct(productId, branchtId));
    }

    /** -------------------------
     *  NEW — DECREMENT STOCK
     * ------------------------- */
    @PostMapping("/decrement")
    public ResponseEntity<ApiResponse> decrementStock(@RequestBody DecrementStockRequest req) {
        inventoryService.decrementStock(req.getProductId(), req.getBranchId(), req.getQuantity(), req.getReference());
        return ResponseEntity.ok(new ApiResponse("success", "Stock decremented"));
    }

    /** -------------------------
     *  NEW — RESERVE STOCK
     * ------------------------- */
    @PreAuthorize("hasAnyRole('SUPERUSER','ADMIN','MANAGER','SUPERVISOR')")
    @PostMapping("/reserve")
    public ResponseEntity<ApiResponse> reserveStock(@RequestBody ReserveStockRequest req) {
        inventoryService.reserveStock(req.getProductId(), req.getBranchId(), req.getQuantity(), req.getReference());
        return ResponseEntity.ok(new ApiResponse("success", "Stock reserved"));
    }

    /** -------------------------
     *  NEW — RELEASE RESERVED STOCK
     * ------------------------- */
    @PreAuthorize("hasAnyRole('SUPERUSER','ADMIN','MANAGER','SUPERVISOR')")
    @PostMapping("/release")
    public ResponseEntity<ApiResponse> releaseReservation(@RequestBody ReleaseStockRequest req) {
        inventoryService.releaseReservation(req.getProductId(), req.getBranchId(), req.getQuantity(), req.getReference());
        return ResponseEntity.ok(new ApiResponse("success", "Reserved stock released"));
    }

    /** -------------------------
     *  NEW — LIST ALL INVENTORY
     * ------------------------- */
    @GetMapping
    public ResponseEntity<ApiResponse> listAllInventory() {
        var items = inventoryService.getAllInventory();
        return ResponseEntity.ok(new ApiResponse("success", "Inventory list", items));
    }

    /** -------------------------
     *  NEW — LIST INVENTORY BY LOCATION
     * ------------------------- */
    @GetMapping("/branch/{branchId}")
    public ResponseEntity<ApiResponse> listByBranch(@PathVariable UUID branchId) {
        var items = inventoryService.getInventoryByBranch(branchId);
        return ResponseEntity.ok(new ApiResponse("success", "Inventory list for branch", items));
    }

    /** -------------------------
     *  NEW — STOCK TRANSACTION HISTORY (ALL)
     * ------------------------- */
    @GetMapping("/transactions")
    public ResponseEntity<ApiResponse> listTransactions() {
        return ResponseEntity.ok(
                new ApiResponse("success",
                        "Transaction list",
                        stockTransactionRepository.findAll())
        );
    }

    /** -------------------------
     *  NEW — STOCK TRANSACTION HISTORY FOR ONE PRODUCT
     * ------------------------- */
    @GetMapping("/transactions/{productId}")
    public ResponseEntity<ApiResponse> listTransactionsForProduct(@PathVariable UUID productId) {
        return ResponseEntity.ok(
                new ApiResponse("success",
                        "Transaction list for product",
                        stockTransactionRepository.findByProductId(productId))
        );
    }
}