package com.IntegrityTechnologies.business_manager.modules.stock.product.parent.controller;

import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.*;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.dto.ProductImageAuditDTO;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.dto.ProductImageDTO;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.service.ProductService;
import lombok.RequiredArgsConstructor;
import org.springframework.core.io.Resource;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import java.io.IOException;
import java.util.List;
import java.util.Map;
import java.util.UUID;

@RestController
@RequestMapping("/api/products/images")
@RequiredArgsConstructor
@TenantUserOnly
public class ProductImageController {

    private final ProductService productService;

    /* ===================== READ ===================== */

    @GetMapping("/{productId}")
    public List<ProductImageDTO> getImages(
            @RequestParam UUID branchId,
            @PathVariable UUID productId,
            @RequestParam(required = false)
            Boolean deleted
    ) {

        return productService.getProductImages(
                        branchId,
                        productId,
                        deleted
                );
    }

    @GetMapping("/{id}/thumbnail")
    public ResponseEntity<Resource> thumbnail(
            @RequestParam(required = false) UUID branchId,
            @PathVariable UUID id
    ) {
        return productService.getProductThumbnail(
                branchId,
                id
        );
    }

    @GetMapping("/{productId}/zip")
    public ResponseEntity<Resource> downloadImages(
            @PathVariable UUID productId,
            @RequestParam(required = false) UUID branchId,
            @RequestParam(required = false) Boolean deleted
    ) throws IOException {
        return productService.downloadProductImagesZip(branchId, productId, deleted);
    }

    @TenantManagerOnly
    @GetMapping
    public Map<UUID, List<String>> getAllImages(
            @RequestParam(required = false) UUID branchId
    ) {
        return productService.getAllProductImageUrls(branchId);
    }

    @GetMapping("/shared/{fileName:.+}")
    public ResponseEntity<Resource> getSharedImage(
            @RequestParam(required = false) UUID branchId,
            @PathVariable String fileName
    ) {
        return productService.getSharedProductImage(
                branchId,
                fileName
        );
    }

    @TenantManagerOnly
    @GetMapping("/zip")
    public ResponseEntity<Resource> downloadAllImages(
            @RequestParam(required = false) UUID branchId,
            @RequestParam(required = false) Boolean deletedProducts,
            @RequestParam(required = false) Boolean deletedImages
    ) throws IOException {
        return productService.downloadAllProductImagesZip(branchId, deletedProducts, deletedImages);
    }

    /* ===================== WRITE ===================== */

    @TenantSupervisorOnly
    @PatchMapping("/{productId}")
    public void uploadImages(
            @PathVariable UUID productId,
            @RequestParam(required = false) UUID branchId,
            @RequestParam List<MultipartFile> files
    ) throws IOException {
        productService.uploadProductImages(branchId, productId, files);
    }

    @TenantSupervisorOnly
    @DeleteMapping("/{productId}/{filename}")
    public void deleteImage(
            @PathVariable UUID productId,
            @PathVariable String filename,
            @RequestParam(required = false) UUID branchId,
            @RequestParam(defaultValue = "true") Boolean soft
    ) throws IOException {
        productService.deleteProductImageByFilename(branchId, productId, filename, soft);
    }

    @TenantSupervisorOnly
    @DeleteMapping("/{productId}")
    public void deleteAllImages(
            @PathVariable UUID productId,
            @RequestParam(required = false) UUID branchId,
            @RequestParam(defaultValue = "true") Boolean soft,
            @RequestParam(required = false) String reason
    ) throws IOException {
        productService.deleteAllProductImages(branchId, productId, soft, reason);
    }

    @TenantSupervisorOnly
    @PutMapping("/{productId}/{imageId}/restore")
    public void restoreImage(
            @PathVariable UUID productId,
            @PathVariable UUID imageId,
            @RequestParam(required = false) UUID branchId,
            @RequestParam(required = false) String reason
    ) {
        productService.restoreProductImage(branchId, productId, imageId, reason);
    }

    @GetMapping("/{productId}/audits")
    public List<ProductImageAuditDTO> getImageAudits(
            @RequestParam UUID branchId,
            @PathVariable UUID productId
    ) {
        return productService.getProductImageAudits(
                branchId,
                productId
        );
    }
}