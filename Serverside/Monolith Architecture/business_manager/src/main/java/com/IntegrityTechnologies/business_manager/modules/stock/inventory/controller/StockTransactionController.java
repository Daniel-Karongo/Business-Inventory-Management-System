package com.IntegrityTechnologies.business_manager.modules.stock.inventory.controller;

import com.IntegrityTechnologies.business_manager.config.response.ApiResponse;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.service.StockTransactionService;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

import java.util.UUID;

@RestController
@RequestMapping("/api/stock/transactions")
@RequiredArgsConstructor
public class StockTransactionController {

    private final StockTransactionService service;

    @GetMapping("/branch/{branchId}")
    public ApiResponse all(@PathVariable UUID branchId) {
        return new ApiResponse("success", "Transactions", service.getAll(branchId));
    }

    @GetMapping("/branch/{branchId}/variant/{variantId}")
    public ApiResponse byVariant(
            @PathVariable UUID branchId,
            @PathVariable UUID variantId
    ) {
        return new ApiResponse("success", "Transactions",
                service.getByVariant(variantId, branchId));
    }

    @GetMapping("/branch/{branchId}/product/{productId}")
    public ApiResponse byProduct(
            @PathVariable UUID branchId,
            @PathVariable UUID productId
    ) {
        return new ApiResponse("success", "Transactions",
                service.getByProduct(productId, branchId));
    }
}