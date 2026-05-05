package com.IntegrityTechnologies.business_manager.modules.stock.inventory.controller;

import com.IntegrityTechnologies.business_manager.config.response.ApiResponse;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.service.valuation.InventoryValuationService;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

import java.time.LocalDate;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

@RestController
@RequestMapping("/api/stock/valuation")
@RequiredArgsConstructor
public class ValuationController {

    private final InventoryValuationService valuationService;

    /* ===================== BASIC ===================== */

    @GetMapping
    public ApiResponse total() {
        return new ApiResponse(
                "success",
                "Inventory valuation",
                valuationService.getTotalValuation()
        );
    }

    @GetMapping("/dashboard")
    public ApiResponse dashboard() {
        Map<String, Object> res = new HashMap<>();

        res.put("valuationMethod", valuationService.resolveCurrentMethod());
        res.put("totalValuation",
                valuationService.getTotalValuation().get("totalValuation"));
        res.put("branchValuation", valuationService.getAllBranchesValuation());
        res.put("categoryValuation", valuationService.getCategoryValuation());
        res.put("topProducts", valuationService.getTopValuedProducts(10));

        return new ApiResponse("success", "Valuation dashboard", res);
    }

    /* ===================== SCOPED ===================== */

    @GetMapping("/product/{productId}")
    public ApiResponse product(@PathVariable UUID productId) {
        return new ApiResponse(
                "success",
                "Product valuation",
                valuationService.getProductValuation(productId)
        );
    }

    @GetMapping("/branch/{branchId}")
    public ApiResponse branch(@PathVariable UUID branchId) {
        return new ApiResponse(
                "success",
                "Branch valuation",
                valuationService.getBranchValuation(branchId)
        );
    }

    @GetMapping("/categories")
    public ApiResponse categories() {
        return new ApiResponse(
                "success",
                "Category valuation",
                valuationService.getCategoryValuation()
        );
    }

    /* ===================== HISTORY ===================== */

    @GetMapping("/history")
    public ApiResponse history(
            @RequestParam(required = false) LocalDate date,
            @RequestParam(required = false) String method
    ) {
        LocalDate d = (date == null) ? LocalDate.now() : date;

        return new ApiResponse(
                "success",
                "Historical valuation for " + d,
                valuationService.getHistoricalValuation(d, method)
        );
    }

    @GetMapping("/history/{variantId}")
    public ApiResponse historyVariant(
            @PathVariable UUID variantId,
            @RequestParam UUID branchId,
            @RequestParam(required = false) LocalDate date
    ) {
        LocalDate d = (date == null) ? LocalDate.now() : date;

        return new ApiResponse(
                "success",
                "Historical valuation for " + d,
                valuationService.getHistoricalValuation(variantId, branchId, d)
        );
    }
}