package com.IntegrityTechnologies.business_manager.modules.stock.product.parent.controller;

import com.IntegrityTechnologies.business_manager.config.response.ApiResponse;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.dto.ProductRestoreOptions;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.dto.*;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.service.ProductService;
import lombok.RequiredArgsConstructor;
import org.springframework.core.io.Resource;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import java.io.IOException;
import java.util.List;
import java.util.UUID;

@RestController
@RequestMapping("/api/products")
@RequiredArgsConstructor
public class ProductController {

    private final ProductService productService;

    /* ===================== READ ===================== */

    @GetMapping
    public ApiResponse getAll(
            @RequestParam UUID branchId,
            @RequestParam(required = false) Boolean deleted
    ) {
        return new ApiResponse(
                "success",
                "Products fetched",
                productService.getAllProducts(branchId, deleted)
        );
    }

    @GetMapping("/{id}")
    public ApiResponse getById(
            @PathVariable UUID id,
            @RequestParam UUID branchId,
            @RequestParam(required = false) Boolean deleted
    ) {
        return new ApiResponse(
                "success",
                "Product fetched",
                productService.getProductById(branchId, id, deleted)
        );
    }

    @GetMapping("/sku/{sku}")
    public ApiResponse getBySku(
            @PathVariable String sku,
            @RequestParam UUID branchId,
            @RequestParam(required = false) Boolean deleted
    ) {
        return new ApiResponse(
                "success",
                "Product fetched",
                productService.getProductBySKU(branchId, sku, deleted)
        );
    }

    @GetMapping("/category/{categoryId}")
    public ApiResponse byCategory(
            @PathVariable Long categoryId,
            @RequestParam UUID branchId,
            @RequestParam(required = false) Boolean deleted,
            @RequestParam(defaultValue = "false") Boolean strict
    ) {
        return new ApiResponse(
                "success",
                "Products by category",
                productService.getProductsByCategory(branchId, categoryId, deleted, strict)
        );
    }

    @GetMapping("/supplier/{supplierId}")
    public ApiResponse bySupplier(
            @PathVariable UUID supplierId,
            @RequestParam UUID branchId,
            @RequestParam(required = false) Boolean deleted
    ) {
        return new ApiResponse(
                "success",
                "Products by supplier",
                productService.getProductsBySupplier(branchId, supplierId, deleted)
        );
    }

    /* ===================== CREATE ===================== */
    @PostMapping(
            value = "",
            consumes = MediaType.MULTIPART_FORM_DATA_VALUE
    )
    public ApiResponse createFull(
            @RequestParam UUID branchId,
            @RequestPart("payload") ProductFullCreateDTO dto,
            @RequestPart(value = "files", required = false)
            List<MultipartFile> files
    ) throws IOException {

        return new ApiResponse(
                "success",
                "Product created",
                productService.fullCreate(branchId, dto, files)
        );
    }

    /* ===================== UPDATE ===================== */

    @PutMapping(
            value = "/{id}",
            consumes = MediaType.MULTIPART_FORM_DATA_VALUE
    )
    public ApiResponse update(
            @PathVariable UUID id,
            @RequestParam UUID branchId,
            @RequestPart("payload")
            ProductFullUpdateDTO dto,
            @RequestPart(
                    value = "files",
                    required = false
            )
            List<MultipartFile> files
    ) throws IOException {

        return new ApiResponse(
                "success",
                "Product updated",
                productService.fullUpdate(
                        branchId,
                        id,
                        dto,
                        files
                )
        );
    }

    /* ===================== DELETE ===================== */

    @DeleteMapping("/{id}/soft")
    public ApiResponse softDelete(
            @PathVariable UUID id,
            @RequestParam UUID branchId,
            @RequestParam(required = false) String reason
    ) {
        productService.softDeleteProduct(
                branchId,
                id,
                reason
        );
        return new ApiResponse("success", "Product soft deleted", null);
    }

    @DeleteMapping("/{id}/hard")
    public ApiResponse hardDelete(
            @PathVariable UUID id,
            @RequestParam UUID branchId,
            @RequestParam(required = false) String reason
    ) {
        productService.hardDeleteProduct(branchId, id, reason);
        return new ApiResponse("success", "Product permanently deleted", null);
    }

    /* ===================== RESTORE ===================== */

    @PostMapping("/{id}/restore")
    public ApiResponse restore(
            @PathVariable UUID id,
            @RequestParam UUID branchId,
            @RequestParam(required = false) String reason,
            @RequestBody(required = false) ProductRestoreOptions options
    ) {
        productService.restoreProduct(
                branchId,
                id,
                reason,
                options
        );
        return new ApiResponse("success", "Product restored", null);
    }

    /* ===================== IMAGES ===================== */

    @GetMapping("/{id}/images")
    public ApiResponse imageUrls(
            @PathVariable UUID id,
            @RequestParam UUID branchId,
            @RequestParam(required = false) Boolean deleted
    ) {
        return new ApiResponse(
                "success",
                "Image URLs",
                productService.getProductImages(branchId, id, deleted)
        );
    }

    @GetMapping("/{id}/images/download")
    public ResponseEntity<Resource> downloadZip(
            @PathVariable UUID id,
            @RequestParam UUID branchId,
            @RequestParam(required = false) Boolean deleted
    ) throws IOException {
        return productService.downloadProductImagesZip(branchId, id, deleted);
    }

    @GetMapping("/{id}/thumbnail")
    public ResponseEntity<Resource> thumbnail(
            @PathVariable UUID id,
            @RequestParam UUID branchId
    ) {
        return productService.getProductThumbnail(branchId, id);
    }

    /* ===================== AUDITS ===================== */

    @GetMapping("/{id}/audits")
    public ApiResponse audits(
            @PathVariable UUID id,
            @RequestParam UUID branchId
    ) {
        return new ApiResponse(
                "success",
                "Audit trail",
                productService.getProductAudits(branchId, id)
        );
    }
}