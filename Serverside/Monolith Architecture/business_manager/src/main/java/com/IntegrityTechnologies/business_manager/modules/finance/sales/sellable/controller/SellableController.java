package com.IntegrityTechnologies.business_manager.modules.finance.sales.sellable.controller;

import com.IntegrityTechnologies.business_manager.config.response.ApiResponse;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.sellable.dto.SellableProductRequest;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.sellable.dto.SellableVariantRequest;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.sellable.service.SellableProductService;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/sellable")
@RequiredArgsConstructor
public class SellableController {

    private final SellableProductService productService;

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

    @PostMapping("/variant")
    public ApiResponse getVariant(
            @RequestBody SellableVariantRequest request
    ) {
        return new ApiResponse(
                "success",
                "Sellable variant fetched",
                productService.getVariant(request)
        );
    }
}