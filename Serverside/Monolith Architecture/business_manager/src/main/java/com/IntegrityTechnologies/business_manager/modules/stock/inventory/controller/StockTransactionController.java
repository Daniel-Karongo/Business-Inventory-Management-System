package com.IntegrityTechnologies.business_manager.modules.stock.inventory.controller;

import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantUserOnly;
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
@TenantUserOnly
public class StockTransactionController {

    private final StockTransactionService service;

    @GetMapping("/branch/{branchId}")
    public ResponseEntity<List<StockTransactionDTO>> getAll(
            @PathVariable UUID branchId
    ) {
        return ResponseEntity.ok(service.getAll(branchId));
    }

    @GetMapping("/branch/{branchId}/variant/{variantId}")
    public ResponseEntity<List<StockTransactionDTO>> getByVariant(
            @PathVariable UUID branchId,
            @PathVariable UUID variantId
    ) {
        return ResponseEntity.ok(service.getByVariant(variantId, branchId));
    }

    @GetMapping("/branch/{branchId}/product/{productId}")
    public ResponseEntity<List<StockTransactionDTO>> getByProduct(
            @PathVariable UUID branchId,
            @PathVariable UUID productId
    ) {
        return ResponseEntity.ok(service.getByProduct(productId, branchId));
    }
}