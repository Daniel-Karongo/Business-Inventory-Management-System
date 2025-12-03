package com.IntegrityTechnologies.business_manager.modules.finance.sales.controller;

import com.IntegrityTechnologies.business_manager.modules.finance.payment.dto.PaymentResponse;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.dto.SaleRequest;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.dto.SaleResponse;
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

    @PostMapping
    public ResponseEntity<SaleResponse> createSale(@RequestBody SaleRequest req) {
        return ResponseEntity.ok(salesService.createSale(req));
    }

    /** NEW — Get sale by ID */
    @GetMapping("/{id}")
    public ResponseEntity<SaleResponse> getSale(@PathVariable UUID id) {
        return ResponseEntity.ok(salesService.getSale(id));
    }

    /** NEW — List sales with filters */
    @GetMapping
    public ResponseEntity<Page<SaleResponse>> listSales(
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "20") int size,
            @RequestParam(required = false) String status,
            @RequestParam(required = false) String customer,
            @RequestParam(required = false) UUID branchId,
            @RequestParam(required = false) LocalDate from,
            @RequestParam(required = false) LocalDate to
    ) {
        return ResponseEntity.ok(
                salesService.listSales(page, size, status, customer, branchId, from, to));
    }

    /** NEW — Cancel sale */
    @PostMapping("/{id}/cancel")
    public ResponseEntity<SaleResponse> cancelSale(@PathVariable UUID id) {
        return ResponseEntity.ok(salesService.cancelSale(id));
    }

    /** NEW — Update sale (before payment) */
    @PutMapping("/{id}")
    public ResponseEntity<SaleResponse> updateSale(
            @PathVariable UUID id,
            @RequestBody SaleRequest req
    ) {
        return ResponseEntity.ok(salesService.updateSale(id, req));
    }

    /** NEW — Get payments for a sale */
    @GetMapping("/{id}/payments")
    public ResponseEntity<Iterable<PaymentResponse>> getSalePayments(@PathVariable UUID id) {
        return ResponseEntity.ok(salesService.getSalePayments(id));
    }

    /** NEW — Receipt endpoint */
    @GetMapping("/{id}/receipt")
    public ResponseEntity<SaleResponse> getReceipt(@PathVariable UUID id) {
        return ResponseEntity.ok(salesService.getReceipt(id));
    }
}