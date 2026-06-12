package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.controller;

import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantManagerOnly;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantUserOnly;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.dto.VariantImageAuditDTO;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.dto.VariantImageDTO;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.service.ProductVariantImageService;
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

    @GetMapping("/{variantId}/images/all")
    public List<VariantImageDTO> getAllImages(
            @RequestParam UUID branchId,
            @PathVariable UUID variantId
    ) {
        return service.getAllImages(
                branchId,
                variantId
        );
    }

    @GetMapping("/{variantId}/thumbnails/{fileName:.+}")
    public ResponseEntity<Resource> getThumbnail(
            @RequestParam UUID branchId,
            @PathVariable UUID variantId,
            @PathVariable String fileName
    ) {
        return service.getThumbnail(
                branchId,
                variantId,
                fileName
        );
    }

    @GetMapping("/thumbnails/shared/{fileName:.+}")
    public ResponseEntity<Resource> getSharedThumbnail(
            @RequestParam UUID branchId,
            @PathVariable String fileName
    ) {
        return service.getSharedThumbnail(
                branchId,
                fileName
        );
    }

    @GetMapping("/{variantId}/images/{fileName:.+}")
    public ResponseEntity<Resource> getVariantImage(
            @RequestParam UUID branchId,
            @PathVariable UUID variantId,
            @PathVariable String fileName
    ) {
        return service.getProductVariantImage(
                branchId,
                variantId,
                fileName
        );
    }

    @TenantManagerOnly
    @PostMapping("/{variantId}/images")
    public ResponseEntity<Void> upload(
            @RequestParam UUID branchId,
            @PathVariable UUID variantId,
            @RequestParam("files") List<MultipartFile> files
    ) throws IOException {

        service.uploadVariantImages(
                branchId,
                variantId,
                files
        );

        return ResponseEntity.noContent().build();
    }

    @GetMapping("/{variantId}/images/zip")
    public ResponseEntity<Resource> downloadVariantImagesZip(
            @RequestParam UUID branchId,
            @PathVariable UUID variantId
    ) throws IOException {

        return service.downloadZipResponse(
                branchId,
                variantId
        );
    }

    @TenantManagerOnly
    @DeleteMapping("/{variantId}/images/{fileName:.+}")
    public ResponseEntity<Void> deleteImage(
            @RequestParam UUID branchId,
            @RequestParam(required = false) String reason,
            @PathVariable UUID variantId,
            @PathVariable String fileName
    ) {
        service.deleteVariantImage(
                branchId,
                variantId,
                fileName,
                reason
        );

        return ResponseEntity.noContent().build();
    }

    @TenantManagerOnly
    @PostMapping("/{variantId}/images/{fileName:.+}/restore")
    public ResponseEntity<Void> restoreImage(
            @RequestParam UUID branchId,
            @RequestParam(required = false) String reason,
            @PathVariable UUID variantId,
            @PathVariable String fileName
    ) {
        service.restoreVariantImage(
                branchId,
                variantId,
                fileName,
                reason
        );

        return ResponseEntity.noContent().build();
    }

    @TenantManagerOnly
    @DeleteMapping("/{variantId}/images/{fileName:.+}/hard")
    public ResponseEntity<Void> hardDeleteImage(
            @RequestParam UUID branchId,
            @RequestParam(required = false) String reason,
            @PathVariable UUID variantId,
            @PathVariable String fileName
    ) {
        service.hardDeleteVariantImage(
                branchId,
                variantId,
                fileName,
                reason
        );

        return ResponseEntity.noContent().build();
    }

    @GetMapping("/{variantId}/image-audits")
    public List<VariantImageAuditDTO> getImageAuditHistory(
            @RequestParam UUID branchId,
            @PathVariable UUID variantId
    ) {
        return service.getImageAuditHistory(
                branchId,
                variantId
        );
    }
}