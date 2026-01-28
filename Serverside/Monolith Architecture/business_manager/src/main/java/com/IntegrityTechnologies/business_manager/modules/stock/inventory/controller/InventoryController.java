package com.IntegrityTechnologies.business_manager.modules.stock.inventory.controller;

import com.IntegrityTechnologies.business_manager.common.ApiResponse;
import com.IntegrityTechnologies.business_manager.config.OptimisticRetryRunner;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.dto.*;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.service.InventoryService;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository.StockTransactionRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.service.InventoryValuationService;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.bind.annotation.*;

import java.time.LocalDate;
import java.util.*;

@Tag(name = "Inventory")
@RestController
@RequestMapping("/api/inventory")
@RequiredArgsConstructor
public class InventoryController {

    private final InventoryService inventoryService;
    private final StockTransactionRepository stockTransactionRepository;
    private final InventoryValuationService valuationService;

    /** -------------------------
     * RECEIVE
     * ------------------------- */

    @PreAuthorize("hasAnyRole('SUPERUSER','ADMIN','MANAGER','SUPERVISOR')")
    @PostMapping("/receive")
    public ResponseEntity<ApiResponse> receiveStock(@RequestBody ReceiveStockRequest req) {
        return ResponseEntity.ok(
                OptimisticRetryRunner.runWithRetry(() -> inventoryService.receiveStock(req))
        );
    }

    @PreAuthorize("hasAnyRole('SUPERUSER','ADMIN','MANAGER','SUPERVISOR')")
    @PostMapping("/receive/bulk")
    @Transactional
    public ResponseEntity<ApiResponse> bulkReceiveStock(@RequestBody BulkReceiveStockRequest req) {
        List<ReceiveStockRequest> stockRequests = req.getItems();
        List<Object> responses = new ArrayList<>();
        for (ReceiveStockRequest stockRequest : stockRequests) {
            OptimisticRetryRunner.runWithRetry(() -> responses.add(inventoryService.receiveStock(stockRequest).getData()));
        }
        return ResponseEntity.ok(new ApiResponse("success", "Inventory Updated successfully", responses));
    }

    @PreAuthorize("hasAnyRole('SUPERUSER','ADMIN','MANAGER','SUPERVISOR')")
    @PostMapping("/transfer")
    public ResponseEntity<ApiResponse> transferStock(
            @RequestBody TransferStockRequest req
    ) {
        return ResponseEntity.ok(
                OptimisticRetryRunner.runWithRetry(
                        () -> inventoryService.transferStock(req)
                )
        );
    }

    /** -------------------------
     * ADJUST
     * ------------------------- */
    @PreAuthorize("hasAnyRole('SUPERUSER','ADMIN','MANAGER','SUPERVISOR')")
    @PostMapping("/adjust/variant")
    public ResponseEntity<ApiResponse> adjustStockVariant(@RequestBody AdjustStockRequest req) {
        return ResponseEntity.ok(inventoryService.adjustStockVariant(req));
    }

    /** -------------------------
     * DECREMENT / RESERVE / RELEASE (variant-first)
     * ------------------------- */
    @PostMapping("/variant/decrement")
    public ResponseEntity<ApiResponse> decrementVariantStock(@RequestBody DecrementStockRequest req) {
        // expects productVariantId in request
        inventoryService.decrementVariantStock(req.getProductVariantId(), req.getBranchId(), req.getQuantity(), req.getReference());
        return ResponseEntity.ok(new ApiResponse("success", "Variant stock decremented"));
    }

    @PreAuthorize("hasAnyRole('SUPERUSER','ADMIN','MANAGER','SUPERVISOR')")
    @PostMapping("/variant/reserve")
    public ResponseEntity<ApiResponse> reserveVariantStock(@RequestBody ReserveStockRequest req) {
        inventoryService.reserveStockVariant(req.getProductVariantId(), req.getBranchId(), req.getQuantity(), req.getReference());
        return ResponseEntity.ok(new ApiResponse("success", "Variant stock reserved"));
    }

    @PreAuthorize("hasAnyRole('SUPERUSER','ADMIN','MANAGER','SUPERVISOR')")
    @PostMapping("/variant/release")
    public ResponseEntity<ApiResponse> releaseVariantReservation(@RequestBody ReleaseStockRequest req) {
        inventoryService.releaseReservationVariant(req.getProductVariantId(), req.getBranchId(), req.getQuantity(), req.getReference());
        return ResponseEntity.ok(new ApiResponse("success", "Variant reservation released"));
    }

    /** -------------------------
     * READS — variant and product
     * ------------------------- */

    @GetMapping("/variant/{variantId}")
    public ResponseEntity<ApiResponse> getVariantInventoryAcrossBranches(@PathVariable UUID variantId) {
        return ResponseEntity.ok(inventoryService.getInventoryForVariantBranch(variantId, null));
    }

    @GetMapping("/variant/{variantId}/branch/{branchId}")
    public ResponseEntity<ApiResponse> getVariantInventoryForBranch(@PathVariable UUID variantId, @PathVariable UUID branchId) {
        return ResponseEntity.ok(inventoryService.getInventoryForVariantBranch(variantId, branchId));
    }

    /** legacy-style list endpoints but variant-aware */
    @GetMapping
    public ResponseEntity<ApiResponse> listAllInventory() {
        var items = inventoryService.getAllInventory();
        return ResponseEntity.ok(new ApiResponse("success", "Inventory list", items));
    }

    @GetMapping("/branch/{branchId}")
    public ResponseEntity<ApiResponse> listByBranch(@PathVariable UUID branchId) {
        var items = inventoryService.getInventoryByBranch(branchId);
        return ResponseEntity.ok(new ApiResponse("success", "Inventory list for branch", items));
    }

    /** -------------------------
     * Reports & misc
     * ------------------------- */

    @GetMapping("/product/stock-across-branches/{productId}")
    public ResponseEntity<ApiResponse> productStockAcrossBranches(@PathVariable UUID productId) {
        return ResponseEntity.ok(new ApiResponse("success", "Stock across branches", inventoryService.getProductStockAcrossBranches(productId)));
    }

    @GetMapping("/product/{productId}/branch/{branchId}")
    public ResponseEntity<ApiResponse> productStockInBranch(
            @PathVariable UUID productId,
            @PathVariable UUID branchId
    ) {
        return ResponseEntity.ok(new ApiResponse("success", "Stock across branches", inventoryService.getProductStockInBranch(productId, branchId)));
    }

    @GetMapping("/low-stock")
    public ResponseEntity<ApiResponse> lowStock(@RequestParam(defaultValue = "10") Long threshold) {
        return ResponseEntity.ok(new ApiResponse("success", "Low stock items", inventoryService.getLowStock(threshold)));
    }

    @GetMapping("/out-of-stock")
    public ResponseEntity<ApiResponse> outOfStock() {
        return ResponseEntity.ok(new ApiResponse("success", "Out of stock items", inventoryService.getOutOfStock()));
    }

    @GetMapping("/transactions")
    public ResponseEntity<ApiResponse> listTransactions() {
        return ResponseEntity.ok(new ApiResponse("success", "Transaction list", stockTransactionRepository.findAll()));
    }

    @GetMapping("/transactions/product/{productId}")
    public ResponseEntity<ApiResponse> listTransactionsForProduct(@PathVariable UUID productId) {
        return ResponseEntity.ok(new ApiResponse("success", "Transaction list for product", stockTransactionRepository.findByProductId(productId)));
    }

    @GetMapping("/valuation/dashboard")
    public ResponseEntity<ApiResponse> valuationDashboard() {

        Map<String, Object> response = new HashMap<>();

        response.put("valuationMethod", valuationService.resolveCurrentMethod());

        response.put("totalValuation", valuationService.getTotalValuation().get("totalValuation"));
        response.put("branchValuation", valuationService.getAllBranchesValuation());
        response.put("categoryValuation", valuationService.getCategoryValuation());
        response.put("topProducts", valuationService.getTopValuedProducts(10));

        return ResponseEntity.ok(new ApiResponse("success", "Valuation dashboard", response));
    }

    @GetMapping("/valuation")
    public ResponseEntity<ApiResponse> valuation() {
        return ResponseEntity.ok(new ApiResponse("success", "Inventory valuation", valuationService.getTotalValuation()));
    }

    @GetMapping("/valuation/product/{productId}")
    public ResponseEntity<ApiResponse> valuationByProduct(@PathVariable UUID productId) {
        return ResponseEntity.ok(new ApiResponse("success", "Product valuation", valuationService.getProductValuation(productId)));
    }

    @GetMapping("/valuation/branch/{branchId}")
    public ResponseEntity<ApiResponse> valuationByBranch(@PathVariable UUID branchId) {
        return ResponseEntity.ok(new ApiResponse("success", "Branch valuation", valuationService.getBranchValuation(branchId)));
    }

    @GetMapping("/valuation/history")
    public ResponseEntity<ApiResponse> valuationAsOf(
            @RequestParam(required = false) LocalDate date,
            @RequestParam(required = false) String method
    ) {
        LocalDate d = (date == null) ? LocalDate.now() : date;
        return ResponseEntity.ok(new ApiResponse("success", "Historical valuation for " + d, valuationService.getHistoricalValuation(d, method)));
    }

    @GetMapping("/valuation/categories")
    public ResponseEntity<ApiResponse> valuationByCategory() {
        return ResponseEntity.ok(new ApiResponse("success", "Category-level valuation", valuationService.getCategoryValuation()));
    }

    @PostMapping("/snapshot/take")
    public ResponseEntity<ApiResponse> takeSnapshot(@RequestParam(required = false) LocalDate date) {
        LocalDate d = (date == null) ? LocalDate.now() : date;
        inventoryService.takeSnapshot(d);
        return ResponseEntity.ok(new ApiResponse("success", "Snapshot taken"));
    }

    @GetMapping("/snapshot")
    public ResponseEntity<ApiResponse> getSnapshot(@RequestParam LocalDate date) {
        return ResponseEntity.ok(new ApiResponse("success", "Snapshot for " + date, inventoryService.getSnapshot(date)));
    }

    @GetMapping("/audit/{productId}")
    public ResponseEntity<ApiResponse> audit(@PathVariable UUID productId) {
        return ResponseEntity.ok(new ApiResponse("success", "Inventory audit trail", inventoryService.getAuditTrail(productId)));
    }
}
