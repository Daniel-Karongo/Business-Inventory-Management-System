package com.IntegrityTechnologies.business_manager.modules.finance.sales.controller;

import com.IntegrityTechnologies.business_manager.common.bulk.BulkRequest;
import com.IntegrityTechnologies.business_manager.common.bulk.BulkResult;
import com.IntegrityTechnologies.business_manager.config.OptimisticRetryRunner;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.dto.PaymentDTO;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.dto.*;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.service.SaleBulkService;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.service.SalesService;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.time.LocalDate;
import java.util.UUID;

@Tag(name = "Sales")
@RestController
@RequestMapping("/api/sales")
@RequiredArgsConstructor
public class SalesController {

    private final SalesService salesService;
    private final SaleBulkService saleBulkService;

    /* ============================================================
       CREATE SALE
       ============================================================ */

    @PostMapping("/import")
    public ResponseEntity<BulkResult<SaleBulkPreviewRow>> importSales(
            @RequestParam SaleImportMode mode,
            @RequestBody BulkRequest<SaleBulkRow> request
    ) {
        return ResponseEntity.ok(
                saleBulkService.importSales(request, mode)
        );
    }

    /* ============================================================
   DELIVER SALE (Triggers revenue in DELIVERY mode)
   ============================================================ */
    @PostMapping("/{id}/deliver")
    public ResponseEntity<SaleDTO> deliverSale(@PathVariable UUID id) {
        return ResponseEntity.ok(
                OptimisticRetryRunner.runWithRetry(() -> salesService.deliverSale(id))
        );
    }
    @PostMapping
    public ResponseEntity<SaleDTO> createSale(@RequestBody SaleRequest req) {
        return ResponseEntity.ok(salesService.createSale(req));
    }

    /* ============================================================
       GET SALE SUMMARY
       ============================================================ */
    @GetMapping("/{id}")
    public ResponseEntity<SaleDTO> getSale(@PathVariable UUID id) {
        return ResponseEntity.ok(salesService.getSale(id));
    }

    /* ============================================================
       LIST SALES
       ============================================================ */
    @GetMapping
    public ResponseEntity<Page<SaleDTO>> listSales(
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "20") int size,
            @RequestParam(required = false) String status,
            @RequestParam(required = false) String customer,
            @RequestParam(required = false) UUID branchId,
            @RequestParam(required = false) LocalDate from,
            @RequestParam(required = false) LocalDate to
    ) {
        return ResponseEntity.ok(
                salesService.listSales(page, size, status, customer, branchId, from, to)
        );
    }

    /* ============================================================
       UPDATE SALE (only in CREATED state)
       ============================================================ */
    @PutMapping("/{id}")
    public ResponseEntity<SaleDTO> updateSale(
            @PathVariable UUID id,
            @RequestBody SaleRequest req
    ) {

        return ResponseEntity.ok(
                OptimisticRetryRunner.runWithRetry(() -> salesService.updateSale(id, req))
        );
    }

    /* ============================================================
       CANCEL SALE (no refund, only if CREATED)
       ============================================================ */
    @PostMapping("/{id}/cancel")
    public ResponseEntity<SaleDTO> cancelSale(@PathVariable UUID id) {
        return ResponseEntity.ok(
                OptimisticRetryRunner.runWithRetry(() -> salesService.cancelSale(id))
        );
    }

    /* ============================================================
       REFUND SALE (FULL REFUND)
       ============================================================ */
    @PostMapping("/{id}/refund")
    public ResponseEntity<SaleDTO> refundSale(@PathVariable UUID id) {
        return ResponseEntity.ok(
                OptimisticRetryRunner.runWithRetry(() -> salesService.refundSale(id))
        );
    }

    /* ============================================================
       CANCEL + REFUND (FULL WORKFLOW)
       ============================================================ */
    @PostMapping("/{id}/cancel-refund")
    public ResponseEntity<SaleDTO> cancelAndRefundSale(@PathVariable UUID id) {
        return ResponseEntity.ok(
                OptimisticRetryRunner.runWithRetry(() -> salesService.cancelAndRefundSale(id))
        );
    }

    /* ============================================================
       LIST PAYMENTS FOR THIS SALE
       ============================================================ */
    @GetMapping("/{id}/payments")
    public ResponseEntity<Iterable<PaymentDTO>> getSalePayments(@PathVariable UUID id) {
        return ResponseEntity.ok(salesService.getSalePayments(id));
    }

    /* ============================================================
       RECEIPT ENDPOINT
       ============================================================ */
    @GetMapping("/{id}/receipt")
    public ResponseEntity<SaleDTO> getReceipt(@PathVariable UUID id) {
        return ResponseEntity.ok(salesService.getReceipt(id));
    }
}