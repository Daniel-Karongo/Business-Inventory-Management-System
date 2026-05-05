package com.IntegrityTechnologies.business_manager.modules.catalog.controller;

import com.IntegrityTechnologies.business_manager.config.response.ApiResponse;
import com.IntegrityTechnologies.business_manager.modules.catalog.service.CatalogService;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.sellable.dto.SellableProductRequest;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

import java.util.UUID;

@RestController
@RequestMapping("/api/catalog")
@RequiredArgsConstructor
public class CatalogController {

    private final CatalogService catalogService;

    /* =========================
       PRODUCT LIST (STRUCTURAL)
       ========================= */
    @GetMapping("/products")
    public ApiResponse getProducts(
            @RequestParam UUID branchId,
            @RequestParam(required = false) Boolean deleted
    ) {
        return new ApiResponse(
                "success",
                "Catalog products",
                catalogService.getProducts(branchId, deleted)
        );
    }

    /* =========================
       PRODUCT DETAIL (WITH VARIANTS)
       ========================= */
    @GetMapping("/products/{productId}")
    public ApiResponse getProductDetail(
            @PathVariable UUID productId,
            @RequestParam UUID branchId
    ) {
        return new ApiResponse(
                "success",
                "Catalog product detail",
                catalogService.getProductDetail(productId, branchId)
        );
    }

    /* =========================
       FULL SELLABLE SEARCH (POWERED)
       ========================= */
    @PostMapping("/search")
    public ApiResponse search(@RequestBody SellableProductRequest request) {
        return new ApiResponse(
                "success",
                "Catalog search",
                catalogService.searchSellable(request)
        );
    }
}