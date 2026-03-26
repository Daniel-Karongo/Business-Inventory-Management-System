package com.IntegrityTechnologies.business_manager.modules.stock.inventory.controller;

import com.IntegrityTechnologies.business_manager.config.response.ApiResponse;
import com.IntegrityTechnologies.business_manager.config.bulk.BulkRequest;
import com.IntegrityTechnologies.business_manager.config.bulk.BulkResult;
import com.IntegrityTechnologies.business_manager.config.spring.OptimisticRetryRunner;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.*;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.dto.*;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository.StockTransactionRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.service.InventoryBulkService;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.service.InventoryService;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.service.valuation.InventoryValuationService;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Pageable;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.time.LocalDate;
import java.util.*;

@Tag(name = "Inventory")
@RestController
@RequestMapping("/api/inventory")
@RequiredArgsConstructor
@TenantUserOnly
public class InventoryController {

    private final InventoryService inventoryService;
    private final StockTransactionRepository stockTransactionRepository;
    private final InventoryValuationService valuationService;
    private final InventoryBulkService bulkService;

    private UUID tenantId() {
        return TenantContext.getTenantId();
    }

    /* ==================================== IMPORT ==================================== */

    @TenantSupervisorOnly
    @PostMapping("/import")
    public ResponseEntity<BulkResult<InventoryBulkPreviewResult>> importInventory(
            @RequestBody BulkRequest<InventoryReceiveBulkRow> request
    ) {
        return ResponseEntity.ok(bulkService.bulkReceive(request));
    }

    /* ==================================== RECEIVE ==================================== */

    @TenantSupervisorOnly
    @PostMapping("/receive")
    public ResponseEntity<ApiResponse> receiveStock(@RequestBody ReceiveStockRequest req) {
        return ResponseEntity.ok(
                OptimisticRetryRunner.runWithRetry(() -> inventoryService.receiveStock(req))
        );
    }

    /* ==================================== TRANSFER ==================================== */

    @TenantSupervisorOnly
    @PostMapping("/transfer")
    public ResponseEntity<ApiResponse> transferStock(@RequestBody TransferStockRequest req) {
        return ResponseEntity.ok(
                OptimisticRetryRunner.runWithRetry(() -> inventoryService.transferStock(req))
        );
    }

    /* ==================================== ADJUST ==================================== */

    @TenantSupervisorOnly
    @PostMapping("/adjust/variant")
    public ResponseEntity<ApiResponse> adjustStockVariant(@RequestBody AdjustStockRequest req) {
        return ResponseEntity.ok(inventoryService.adjustStockVariant(req));
    }

    /* ==================================== VARIANT OPS ==================================== */

    @PostMapping("/variant/decrement")
    public ResponseEntity<ApiResponse> decrementVariantStock(@RequestBody DecrementStockRequest req) {

        inventoryService.decrementVariantStock(
                req.getProductVariantId(),
                req.getBranchId(),
                req.getQuantity(),
                req.getReference()
        );

        return ResponseEntity.ok(new ApiResponse("success", "Variant stock decremented"));
    }

    @TenantSupervisorOnly
    @PostMapping("/variant/reserve")
    public ResponseEntity<ApiResponse> reserveVariantStock(@RequestBody ReserveStockRequest req) {

        inventoryService.reserveStockVariant(
                req.getProductVariantId(),
                req.getPackagingId(),        // ✅ NEW
                req.getBranchId(),
                req.getBaseUnits(),          // ✅ NEW
                req.getQuantity(),           // sell qty
                req.getReference(),
                req.getBatchSelections()
        );

        return ResponseEntity.ok(new ApiResponse("success", "Variant stock reserved"));
    }

    @TenantSupervisorOnly
    @PostMapping("/variant/release")
    public ResponseEntity<ApiResponse> releaseVariantReservation(@RequestBody ReleaseStockRequest req) {

        inventoryService.releaseReservationVariant(
                req.getBranchId(),
                req.getReference()
        );

        return ResponseEntity.ok(new ApiResponse("success", "Variant reservation released"));
    }

    @PostMapping("/variant/{variantId}/branch/{branchId}/reconcile")
    public ResponseEntity<ApiResponse> reconcile(
            @PathVariable UUID variantId,
            @PathVariable UUID branchId
    ) {

        return ResponseEntity.ok(
                inventoryService.reconcileInventory(variantId, branchId)
        );
    }

    @GetMapping("/variant/{variantId}/branch/{branchId}/available-debug")
    public ResponseEntity<ApiResponse> debugAvailable(
            @PathVariable UUID variantId,
            @PathVariable UUID branchId
    ) {

        long available = inventoryService.availableQuantity(variantId, branchId);
        long reserved = inventoryService.getReservedQuantity(variantId, branchId);

        Map<String, Object> res = new HashMap<>();
        res.put("available", available);
        res.put("reserved", reserved);

        return ResponseEntity.ok(
                new ApiResponse("success", "Debug stock", res)
        );
    }

    /* ==================================== READS ==================================== */

    @GetMapping("/variant/{variantId}/branch/{branchId}/batches")
    public ApiResponse getBatches(@PathVariable UUID variantId, @PathVariable UUID branchId) {
        return new ApiResponse(
                "success",
                "Batches retrieved",
                inventoryService.getBatchesForVariantBranch(variantId, branchId)
        );
    }

    @GetMapping("/batch/{batchId}/consumptions")
    public ApiResponse getBatchConsumptions(@PathVariable UUID batchId) {
        return new ApiResponse(
                "success",
                "Batch consumptions retrieved",
                inventoryService.getBatchConsumptions(batchId)
        );
    }

    @PostMapping("/preview-allocation")
    public ResponseEntity<ApiResponse> previewAllocation(@RequestBody PreviewAllocationRequest req) {

        return ResponseEntity.ok(
                new ApiResponse(
                        "success",
                        "Preview allocation",
                        inventoryService.previewAllocation(
                                req.getVariantId(),
                                req.getBranchId(),
                                req.getQuantity(),
                                req.getSelectedBatchIds()
                        )
                )
        );
    }

    @GetMapping("/variant/{variantId}")
    public ResponseEntity<ApiResponse> getVariantInventoryAcrossBranches(@PathVariable UUID variantId) {

        return ResponseEntity.ok(
                inventoryService.getInventoryForVariantBranch(variantId, null)
        );
    }

    @GetMapping("/variant/{variantId}/branch/{branchId}")
    public ResponseEntity<ApiResponse> getVariantInventoryForBranch(
            @PathVariable UUID variantId,
            @PathVariable UUID branchId
    ) {

        return ResponseEntity.ok(
                inventoryService.getInventoryForVariantBranch(variantId, branchId)
        );
    }

    @GetMapping("/variant/{variantId}/branch/{branchId}/available")
    public ResponseEntity<ApiResponse> getAvailableStock(
            @PathVariable UUID variantId,
            @PathVariable UUID branchId
    ) {

        long available = inventoryService.availableQuantity(variantId, branchId);

        return ResponseEntity.ok(
                new ApiResponse("success", "Available stock", available)
        );
    }

    /* ==================================== LISTS ==================================== */

    @GetMapping
    public ResponseEntity<ApiResponse> listAllInventory(Pageable pageable) {
        return ResponseEntity.ok(
                new ApiResponse("success", "Inventory list", inventoryService.getAllInventory(pageable))
        );
    }

    @GetMapping("/branch/{branchId}")
    public ResponseEntity<ApiResponse> listByBranch(@PathVariable UUID branchId, Pageable pageable) {
        return ResponseEntity.ok(
                new ApiResponse("success", "Inventory list for branch",
                        inventoryService.getInventoryByBranch(branchId, pageable))
        );
    }

    @GetMapping("/variant/{variantId}/branch/{branchId}/suggest-batches")
    public ResponseEntity<ApiResponse> suggestBatches(
            @PathVariable UUID variantId,
            @PathVariable UUID branchId,
            @RequestParam long quantity
    ) {

        if (quantity <= 0) {
            throw new IllegalArgumentException("quantity must be > 0");
        }

        return ResponseEntity.ok(
                new ApiResponse(
                        "success",
                        "Batch suggestion",
                        inventoryService.suggestBatches(variantId, branchId, quantity)
                )
        );
    }

    /* ==================================== REPORTS ==================================== */

    @GetMapping("/product/stock-across-branches/{productId}")
    public ResponseEntity<ApiResponse> productStockAcrossBranches(@PathVariable UUID productId) {

        return ResponseEntity.ok(
                new ApiResponse(
                        "success",
                        "Stock across branches",
                        inventoryService.getProductStockAcrossBranches(productId)
                )
        );
    }

    @GetMapping("/product/{productId}/branch/{branchId}")
    public ResponseEntity<ApiResponse> productStockInBranch(
            @PathVariable UUID productId,
            @PathVariable UUID branchId
    ) {

        return ResponseEntity.ok(
                new ApiResponse(
                        "success",
                        "Stock across branches",
                        inventoryService.getProductStockInBranch(productId, branchId)
                )
        );
    }

    @GetMapping("/low-stock")
    public ResponseEntity<ApiResponse> lowStock(
            @RequestParam(defaultValue = "10") Long threshold,
            Pageable pageable
    ) {
        return ResponseEntity.ok(inventoryService.getLowStock(threshold, pageable));
    }

    @GetMapping("/out-of-stock")
    public ResponseEntity<ApiResponse> outOfStock(Pageable pageable) {
        return ResponseEntity.ok(inventoryService.getOutOfStock(pageable));
    }

    /* ==================================== TRANSACTIONS ==================================== */

    @GetMapping("/transactions")
    public ResponseEntity<ApiResponse> listTransactions(
            @RequestParam UUID branchId,
            Pageable pageable
    ) {

        return ResponseEntity.ok(
                new ApiResponse(
                        "success",
                        "Transaction list",
                        stockTransactionRepository.findAllScoped(
                                tenantId(),
                                branchId,
                                pageable
                        )
                )
        );
    }

    @GetMapping("/transactions/product/{productId}")
    public ResponseEntity<ApiResponse> listTransactionsForProduct(
            @PathVariable UUID productId,
            @RequestParam UUID branchId
    ) {

        return ResponseEntity.ok(
                new ApiResponse(
                        "success",
                        "Transaction list for product",
                        stockTransactionRepository.findByProductScoped(
                                productId,
                                tenantId(),
                                branchId
                        )
                )
        );
    }

    /* ==================================== VALUATION ==================================== */

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
        return ResponseEntity.ok(
                new ApiResponse("success", "Inventory valuation", valuationService.getTotalValuation())
        );
    }

    @GetMapping("/valuation/product/{productId}")
    public ResponseEntity<ApiResponse> valuationByProduct(@PathVariable UUID productId) {
        return ResponseEntity.ok(
                new ApiResponse("success", "Product valuation",
                        valuationService.getProductValuation(productId))
        );
    }

    @GetMapping("/valuation/branch/{branchId}")
    public ResponseEntity<ApiResponse> valuationByBranch(
            @PathVariable UUID branchId
    ) {
        return ResponseEntity.ok(
                new ApiResponse("success", "Branch valuation",
                        valuationService.getBranchValuation(branchId))
        );
    }

    @GetMapping("/valuation/history")
    public ResponseEntity<ApiResponse> valuationAsOf(
            @RequestParam(required = false) LocalDate date,
            @RequestParam(required = false) String method
    ) {

        LocalDate d = (date == null) ? LocalDate.now() : date;

        return ResponseEntity.ok(
                new ApiResponse(
                        "success",
                        "Historical valuation for " + d,
                        valuationService.getHistoricalValuation(d, method)
                )
        );
    }

    @GetMapping("/valuation/history/{variantId}")
    public ResponseEntity<ApiResponse> valuationAsOf(
            @PathVariable UUID variantId,
            @RequestParam UUID branchId,
            @RequestParam(required = false) LocalDate date
    ) {
        LocalDate d = (date == null) ? LocalDate.now() : date;

        return ResponseEntity.ok(
                new ApiResponse(
                        "success",
                        "Historical valuation for " + d,
                        valuationService.getHistoricalValuation(variantId, branchId, d)
                )
        );
    }

    @GetMapping("/valuation/categories")
    public ResponseEntity<ApiResponse> valuationByCategory() {
        return ResponseEntity.ok(
                new ApiResponse("success", "Category-level valuation",
                        valuationService.getCategoryValuation())
        );
    }

    /* ==================================== SNAPSHOTS ==================================== */

    @PostMapping("/snapshot/take")
    public ResponseEntity<ApiResponse> takeSnapshot(@RequestParam(required = false) LocalDate date) {

        LocalDate d = (date == null) ? LocalDate.now() : date;

        inventoryService.takeSnapshot(d);

        return ResponseEntity.ok(new ApiResponse("success", "Snapshot taken"));
    }

    @GetMapping("/snapshot")
    public ResponseEntity<ApiResponse> getSnapshot(@RequestParam LocalDate date) {
        return ResponseEntity.ok(
                new ApiResponse("success", "Snapshot for " + date,
                        inventoryService.getSnapshot(date))
        );
    }

    /* ==================================== AUDIT ==================================== */

    @GetMapping("/audit/{productId}")
    public ResponseEntity<ApiResponse> audit(@PathVariable UUID productId) {
        return ResponseEntity.ok(
                new ApiResponse("success", "Inventory audit trail",
                        inventoryService.getAuditTrail(productId))
        );
    }
}