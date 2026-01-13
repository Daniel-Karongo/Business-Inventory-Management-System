package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.controller;

import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.service.ProductImageMigrationService;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.service.ProductVariantImageService;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.service.VariantBarcodeBackfillService;
import lombok.RequiredArgsConstructor;
import org.springframework.core.io.FileSystemResource;
import org.springframework.core.io.Resource;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.UUID;

@RestController
@RequestMapping("/api/product-variants")
@RequiredArgsConstructor
public class ProductVariantImageController {

    private final ProductVariantImageService service;
    private final ProductImageMigrationService migrationService;
    private final VariantBarcodeBackfillService barcodeBackfillService;

    @PostMapping("/{variantId}/images")
    public ResponseEntity<Void> upload(
            @PathVariable UUID variantId,
            @RequestParam("files") List<MultipartFile> files
    ) throws IOException {
        service.uploadVariantImages(variantId, files);
        return ResponseEntity.noContent().build();
    }

    @GetMapping("/{variantId}/images/zip")
    public ResponseEntity<Resource> downloadVariantImagesZip(
            @PathVariable UUID variantId
    ) throws IOException {

        File zip = service.zipVariantImages(variantId);
        Resource res = new FileSystemResource(zip);

        return ResponseEntity.ok()
                .contentType(MediaType.APPLICATION_OCTET_STREAM)
                .header(
                        HttpHeaders.CONTENT_DISPOSITION,
                        "attachment; filename=variant-" + variantId + "-images.zip"
                )
                .body(res);
    }
    
    @PreAuthorize("hasRole('SUPERUSER')")
    @PostMapping("/migrate/product-images-to-variants")
    public ResponseEntity<Void> migrateImages() {
        migrationService.migrate();
        return ResponseEntity.noContent().build();
    }

    @PreAuthorize("hasRole('SUPERUSER')")
    @PostMapping("/barcodes/backfill")
    public ResponseEntity<Void> backfill() {
        barcodeBackfillService.backfillBarcodesAndImages();
        return ResponseEntity.noContent().build();
    }
}