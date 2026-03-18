package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.controller;

import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantManagerOnly;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantUserOnly;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.service.ProductVariantImageService;
import lombok.RequiredArgsConstructor;
import org.springframework.core.io.Resource;
import org.springframework.http.*;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import java.io.IOException;
import java.util.List;
import java.util.UUID;

@RestController
@RequestMapping("/api/product-variants")
@RequiredArgsConstructor
@TenantUserOnly
public class ProductVariantImageController {

    private final ProductVariantImageService service;

    @GetMapping("/{variantId}/images/{fileName:.+}")
    public ResponseEntity<Resource> getVariantImage(
            @PathVariable UUID variantId,
            @PathVariable String fileName
    ) {
        return service.getProductVariantImage(variantId, fileName);
    }

    @TenantManagerOnly
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

        return service.downloadZipResponse(variantId); // move logic to service
    }
}