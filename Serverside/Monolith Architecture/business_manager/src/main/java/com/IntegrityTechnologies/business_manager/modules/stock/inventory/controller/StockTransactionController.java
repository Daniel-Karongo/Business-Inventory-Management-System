package com.IntegrityTechnologies.business_manager.modules.stock.inventory.controller;

import com.IntegrityTechnologies.business_manager.modules.stock.inventory.dto.StockTransactionDTO;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.service.StockTransactionService;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.time.LocalDate;
import java.util.List;
import java.util.UUID;

@RestController
@RequestMapping("/api/stock/transactions")
@RequiredArgsConstructor
@Tag(name = "Stock Transactions")
public class StockTransactionController {

    private final StockTransactionService service;

    @GetMapping("/product/{productId}")
    public ResponseEntity<List<StockTransactionDTO>> getForProduct(
            @PathVariable UUID productId
    ) {
        return ResponseEntity.ok(service.getByProduct(productId));
    }

    @GetMapping("/variant/{variantId}")
    public ResponseEntity<List<StockTransactionDTO>> getForVariant(
            @PathVariable UUID variantId
    ) {
        return ResponseEntity.ok(service.getByVariant(variantId));
    }

    @GetMapping("/branch/{branchId}")
    public ResponseEntity<List<StockTransactionDTO>> getForBranch(
            @PathVariable UUID branchId
    ) {
        return ResponseEntity.ok(service.getByBranch(branchId));
    }

    @GetMapping("/product/{productId}/branch/{branchId}")
    public ResponseEntity<List<StockTransactionDTO>> getForProductInBranch(
            @PathVariable UUID productId,
            @PathVariable UUID branchId
    ) {
        return ResponseEntity.ok(service.getByProductAndBranch(productId, branchId));
    }

    @GetMapping("/variant/{variantId}/branch/{branchId}")
    public ResponseEntity<List<StockTransactionDTO>> getForVariantInBranch(
            @PathVariable UUID variantId,
            @PathVariable UUID branchId
    ) {
        return ResponseEntity.ok(service.getByVariantAndBranch(variantId, branchId));
    }

    @GetMapping("/range")
    public ResponseEntity<List<StockTransactionDTO>> getByDateRange(
            @RequestParam @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate from,
            @RequestParam @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate to
    ) {
        return ResponseEntity.ok(service.getByDateRange(from, to));
    }
}