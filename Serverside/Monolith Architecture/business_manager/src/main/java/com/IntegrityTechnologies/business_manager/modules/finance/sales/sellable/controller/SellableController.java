package com.IntegrityTechnologies.business_manager.modules.finance.sales.sellable.controller;

import com.IntegrityTechnologies.business_manager.config.response.ApiResponse;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.sellable.dto.*;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.sellable.service.SellableApplicationService;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.sellable.service.SellableProductService;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/sellable")
@RequiredArgsConstructor
public class SellableController {

    private final SellableApplicationService resolveService;
    private final SellableProductService productService;

    /* =========================
       RESOLVE (CORE)
       ========================= */
    @PostMapping("/resolve")
    public ApiResponse resolve(@RequestBody SellableResolveRequest request) {
        return new ApiResponse(
                "success",
                "Sellable resolved",
                resolveService.resolve(request)
        );
    }

    /* =========================
       SEARCH (UI GRID)
       ========================= */
    @PostMapping("/search")
    public ApiResponse search(@RequestBody SellableProductRequest request) {
        return new ApiResponse(
                "success",
                "Sellable products fetched",
                productService.search(request)
        );
    }
}