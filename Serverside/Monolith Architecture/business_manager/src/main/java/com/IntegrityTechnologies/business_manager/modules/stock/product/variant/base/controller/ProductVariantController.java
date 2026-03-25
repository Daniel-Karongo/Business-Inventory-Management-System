package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.controller;

import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantAdminOnly;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantManagerOnly;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantUserOnly;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.dto.*;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.service.*;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.core.io.Resource;
import org.springframework.http.HttpHeaders;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.UUID;

@RestController
@RequestMapping("/api/product-variants")
@RequiredArgsConstructor
@Tag(name = "Product Variants")
@TenantUserOnly
public class ProductVariantController {

    private final ProductVariantService service;
    private final VariantBarcodeService barcodeService;
    private final ProductVariantImageService imageService;
    private final BarcodeScanService scanService;
    private final VariantPdfOrchestrationService pdfOrchestrationService;

    @TenantManagerOnly
    @PostMapping
    public ResponseEntity<ProductVariantDTO> create(@RequestBody ProductVariantCreateDTO dto) {
        return ResponseEntity.ok(service.createVariant(dto));
    }

    @GetMapping("/{id}")
    public ResponseEntity<ProductVariantDTO> find(@PathVariable UUID id) {
        return ResponseEntity.ok(service.getVariant(id)); // must be SAFE inside service
    }

    @GetMapping("/product/{productId}")
    public ResponseEntity<List<ProductVariantDTO>> findForProduct(@PathVariable UUID productId) {
        return ResponseEntity.ok(service.getVariantsForProduct(productId));
    }

    @GetMapping("/barcode/{barcode}")
    public ResponseEntity<ProductVariantDTO> findByBarcode(@PathVariable String barcode) {
        return ResponseEntity.ok(barcodeService.getVariantByBarcode(barcode));
    }

    /* 🔒 REMOVE branchId from API */
    @PostMapping("/scan")
    public ResponseEntity<BarcodeScanResponse> scan(@RequestBody BarcodeScanRequest req) {
        return ResponseEntity.ok(
                scanService.scan(req.getBarcode()) // branch resolved internally
        );
    }

    @TenantManagerOnly
    @PostMapping("/{id}/barcode")
    public ResponseEntity<ProductVariantDTO> generateBarcode(@PathVariable UUID id) {
        return ResponseEntity.ok(barcodeService.generateBarcodeIfMissing(id));
    }

    /* 🔥 MOVE FILE SERVING TO SERVICE */
    @GetMapping("/{id}/barcode/image")
    public ResponseEntity<Resource> downloadBarcodeImage(@PathVariable UUID id) {
        return barcodeService.getBarcodeImage(id); // new safe method
    }

    @GetMapping("/{id}/images")
    public ResponseEntity<List<String>> getImages(@PathVariable UUID id) {
        return ResponseEntity.ok(imageService.getImageUrls(id));
    }

    @PostMapping("/barcode/pdf/bulk")
    public ResponseEntity<String> bulkPdf(@RequestBody List<UUID> variantIds) {

        String fileName = pdfOrchestrationService.requestBulkPdf(variantIds);

        return ResponseEntity.accepted().body(fileName);
    }

    @GetMapping("/barcode/pdf/download/{fileName}")
    public ResponseEntity<Resource> downloadPdf(@PathVariable String fileName) {

        Resource resource = pdfOrchestrationService.getPdfResource(fileName);

        return ResponseEntity.ok()
                .header(HttpHeaders.CONTENT_DISPOSITION,
                        "attachment; filename=" + fileName)
                .body(resource);
    }

    @GetMapping("/product/{productId}/barcode/pdf")
    public ResponseEntity<String> productPdf(@PathVariable UUID productId) {

        String fileName = pdfOrchestrationService.requestProductPdf(productId);

        return ResponseEntity.accepted().body(fileName);
    }

    @TenantManagerOnly
    @PutMapping("/{id}")
    public ResponseEntity<ProductVariantDTO> update(
            @PathVariable UUID id,
            @RequestBody ProductVariantUpdateDTO dto
    ) {
        return ResponseEntity.ok(service.updateVariant(id, dto));
    }

    @TenantAdminOnly
    @DeleteMapping("/{id}")
    public ResponseEntity<Void> delete(@PathVariable UUID id) {
        service.deleteVariant(id);
        return ResponseEntity.noContent().build();
    }
}