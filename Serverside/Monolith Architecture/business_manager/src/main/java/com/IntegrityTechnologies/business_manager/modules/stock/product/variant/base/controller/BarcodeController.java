package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.controller;

import com.IntegrityTechnologies.business_manager.config.response.ApiResponse;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.dto.*;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.service.BarcodeScanService;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.service.VariantBarcodeService;
import lombok.RequiredArgsConstructor;
import org.springframework.core.io.Resource;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.UUID;

@RestController
@RequestMapping("/api/barcodes")
@RequiredArgsConstructor
public class BarcodeController {

    private final BarcodeScanService scanService;
    private final VariantBarcodeService barcodeService;

    /* =========================
       SCAN → SELLABLE SNAPSHOT
       ========================= */
    @PostMapping("/scan")
    public ApiResponse scan(@RequestBody BarcodeScanRequest request) {
        return new ApiResponse(
                "success",
                "Barcode scanned",
                scanService.scan(request.getBranchId(), request.getBarcode())
        );
    }

    /* =========================
       DIRECT LOOKUP (RAW VARIANT)
       ========================= */
    @GetMapping("/{barcode}")
    public ApiResponse find(
            @PathVariable String barcode,
            @RequestParam UUID branchId
    ) {
        return new ApiResponse(
                "success",
                "Variant fetched",
                barcodeService.getVariantByBarcode(branchId, barcode)
        );
    }

    /* =========================
       BARCODE IMAGE
       ========================= */
    @GetMapping("/variant/{variantId}/image")
    public ResponseEntity<Resource> image(
            @PathVariable UUID variantId,
            @RequestParam UUID branchId
    ) {
        return barcodeService.getBarcodeImage(branchId, variantId);
    }

    /* =========================
       GENERATE BARCODE
       ========================= */
    @PostMapping("/variant/{variantId}")
    public ApiResponse generate(
            @PathVariable UUID variantId,
            @RequestParam UUID branchId
    ) {
        return new ApiResponse(
                "success",
                "Barcode generated",
                barcodeService.generateBarcodeIfMissing(branchId, variantId)
        );
    }
}