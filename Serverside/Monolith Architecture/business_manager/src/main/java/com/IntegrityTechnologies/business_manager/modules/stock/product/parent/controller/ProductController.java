package com.IntegrityTechnologies.business_manager.modules.stock.product.parent.controller;

import com.IntegrityTechnologies.business_manager.common.ApiResponse;
import com.IntegrityTechnologies.business_manager.common.PageWrapper;
import com.IntegrityTechnologies.business_manager.common.bulk.BulkActionRequest;
import com.IntegrityTechnologies.business_manager.common.bulk.BulkRequest;
import com.IntegrityTechnologies.business_manager.common.bulk.BulkResult;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.*;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.dto.*;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.model.ProductAudit;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.model.ProductImageAudit;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.service.ProductBulkService;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.service.ProductService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.core.io.FileSystemResource;
import org.springframework.core.io.Resource;
import org.springframework.http.*;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import java.io.File;
import java.io.IOException;
import java.math.BigDecimal;
import java.util.List;
import java.util.Map;
import java.util.UUID;

@Tag(name = "Products")
@RestController
@RequestMapping("/api/products")
@RequiredArgsConstructor
@Slf4j
@TenantUserOnly
public class ProductController {

    private final ProductService productService;
    private final ProductBulkService bulkService;

    /* =============================
       BASIC READ
       ============================= */

    @GetMapping("")
    @Operation(summary = "Get all products")
    public ResponseEntity<List<ProductDTO>> getAllProducts(
            @RequestParam(required = false) Boolean deleted
    ) {
        return ResponseEntity.ok(productService.getAllProducts(deleted));
    }

    @GetMapping("/advanced")
    public ResponseEntity<PageWrapper<ProductDTO>> getProductsAdvanced(
            @RequestParam(required = false) List<Long> categoryIds,
            @RequestParam(required = false) Long categoryId,
            @RequestParam(required = false) String name,
            @RequestParam(required = false) String description,
            @RequestParam(required = false) String keyword,
            @RequestParam(required = false) Double minPrice,
            @RequestParam(required = false) Double maxPrice,
            @RequestParam(required = false) Boolean deleted,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "id") String sortBy,
            @RequestParam(defaultValue = "asc") String direction,
            @RequestParam(defaultValue = "false") boolean includeDeleted,
            @RequestParam(required = false) Integer minSuppliers,
            @RequestParam(required = false) Integer maxSuppliers,
            @RequestParam(required = false) UUID supplierId
    ) {

        if (categoryId != null) {
            categoryIds = List.of(categoryId);
        }

        var result = productService.getProductsAdvanced(
                categoryIds,
                name,
                description,
                keyword,
                minPrice != null ? BigDecimal.valueOf(minPrice) : null,
                maxPrice != null ? BigDecimal.valueOf(maxPrice) : null,
                deleted,
                page,
                size,
                sortBy,
                direction,
                includeDeleted,
                minSuppliers,
                maxSuppliers,
                supplierId
        );

        return ResponseEntity.ok(new PageWrapper<>(result));
    }

    @GetMapping("/{id}")
    public ResponseEntity<ProductDTO> getProductById(
            @PathVariable UUID id,
            @RequestParam(required = false) Boolean deleted
    ) {
        return ResponseEntity.ok(productService.getProductById(id, deleted));
    }

    @GetMapping("/sku/{sku}")
    public ResponseEntity<ProductDTO> getProductBySKU(
            @PathVariable String sku,
            @RequestParam(required = false) Boolean deleted
    ) {
        return ResponseEntity.ok(productService.getProductBySKU(sku, deleted));
    }

    @TenantManagerOnly
    @GetMapping("/supplier/{supplierId}")
    public ResponseEntity<List<ProductDTO>> getProductsBySupplier(
            @PathVariable UUID supplierId,
            @RequestParam(required = false) Boolean deleted
    ) {
        return ResponseEntity.ok(productService.getProductsBySupplier(supplierId, deleted));
    }

    @GetMapping("/category/{categoryId}")
    public ResponseEntity<List<ProductDTO>> getProductsByCategory(
            @PathVariable Long categoryId,
            @RequestParam(defaultValue = "true") Boolean strict,
            @RequestParam(required = false) Boolean deleted
    ) {
        return ResponseEntity.ok(productService.getProductsByCategory(categoryId, deleted, strict));
    }

    /* =============================
       IMAGE DOWNLOAD
       ============================= */

    @GetMapping("/{id}/images")
    public ResponseEntity<List<String>> getProductImageUrls(
            @PathVariable UUID id,
            @RequestParam(required = false) Boolean deleted
    ) {
        return ResponseEntity.ok(productService.getProductImageUrls(id, deleted));
    }

    @GetMapping("/{id}/images/zip")
    public ResponseEntity<Resource> downloadProductImagesZip(
            @PathVariable UUID id,
            @RequestParam(required = false) Boolean deleted
    ) throws IOException {

        File zip = productService.zipProductImages(id, deleted);

        Resource res = new FileSystemResource(zip);

        return ResponseEntity.ok()
                .contentType(MediaType.APPLICATION_OCTET_STREAM)
                .header(HttpHeaders.CONTENT_DISPOSITION,
                        "attachment; filename=product-" + id + "-images.zip")
                .body(res);
    }

    @GetMapping("/images/all/zip")
    public ResponseEntity<Resource> downloadAllProductImagesZip(
            @RequestParam(required = false) Boolean deletedProducts,
            @RequestParam(required = false) Boolean deletedImages
    ) throws IOException {

        File zip = productService.zipAllProductImages(deletedProducts, deletedImages);

        Resource res = new FileSystemResource(zip);

        return ResponseEntity.ok()
                .contentType(MediaType.APPLICATION_OCTET_STREAM)
                .header(HttpHeaders.CONTENT_DISPOSITION,
                        "attachment; filename=all-product-images.zip")
                .body(res);
    }

    @TenantManagerOnly
    @GetMapping("/images/all")
    public ResponseEntity<Map<UUID, List<String>>> getAllProductImageUrls() {
        return ResponseEntity.ok(productService.getAllProductImageUrls());
    }

    @GetMapping("/{id}/thumbnail")
    public ResponseEntity<Resource> getThumbnail(@PathVariable UUID id) {
        return productService.getProductThumbnail(id);
    }

    /* =============================
       CREATE / UPDATE
       ============================= */

    @TenantManagerOnly
    @PostMapping("/import")
    public ResponseEntity<BulkResult<ProductDTO>> importProducts(
            @RequestBody BulkRequest<ProductBulkRow> request
    ) {
        return ResponseEntity.ok(bulkService.importProducts(request));
    }

    @TenantManagerOnly
    @PostMapping(
            value = "/bulk/full-create",
            consumes = MediaType.MULTIPART_FORM_DATA_VALUE
    )
    public ResponseEntity<BulkResult<ProductDTO>> bulkFullCreate(
            @RequestPart("payload") ProductBulkFrontendRequestDTO dto,
            @RequestPart(value = "files", required = false) List<MultipartFile> files
    ) throws IOException {

        return ResponseEntity.ok(
                bulkService.bulkFullCreateFrontend(dto, files)
        );
    }

    @TenantSupervisorOnly
    @PostMapping(value = "/full-create", consumes = MediaType.MULTIPART_FORM_DATA_VALUE)
    public ProductDTO fullCreate(
            @RequestPart("payload") ProductFullCreateDTO dto,
            @RequestPart(value = "files", required = false) List<MultipartFile> files
    ) throws IOException {

        return productService.fullCreate(dto, files);
    }

    @TenantSupervisorOnly
    @PostMapping(value="/create", consumes = MediaType.MULTIPART_FORM_DATA_VALUE)
    public ResponseEntity<ProductDTO> createProduct(
            @ModelAttribute("product") ProductCreateDTO dto
    ) throws IOException {

        ProductDTO created = productService.createProduct(dto);

        return ResponseEntity.status(HttpStatus.CREATED).body(created);
    }

    @TenantSupervisorOnly
    @PatchMapping(value = "/{id}")
    public ResponseEntity<ProductDTO> updateProduct(
            @PathVariable UUID id,
            @RequestBody ProductUpdateDTO dto
    ) throws IOException {

        return ResponseEntity.ok(productService.updateProduct(id, dto));
    }

    /* =============================
       IMAGE MANAGEMENT
       ============================= */

    @TenantSupervisorOnly
    @PatchMapping("/{id}/images")
    public ResponseEntity<Void> uploadImages(
            @PathVariable UUID id,
            @RequestParam("files") List<MultipartFile> files
    ) throws IOException {

        productService.uploadProductImages(id, files);

        return ResponseEntity.noContent().build();
    }

    /* =============================
       DELETE / RESTORE
       ============================= */

    @PlatformAdminOnly
    @DeleteMapping("/hard/{id}")
    public ResponseEntity<Void> hardDeleteProduct(
            @PathVariable UUID id,
            @RequestParam(required = false) String reason
    ) throws IOException {

        productService.hardDeleteProduct(id, reason);

        return ResponseEntity.noContent().build();
    }

    @TenantSupervisorOnly
    @DeleteMapping("/soft/{id}")
    public ResponseEntity<ApiResponse> softDeleteProduct(
            @PathVariable UUID id,
            @RequestParam(required = false) String reason
    ) {

        productService.bulkSoftDelete(List.of(id), reason);

        return ResponseEntity.ok(new ApiResponse("success", reason));
    }

    @TenantSupervisorOnly
    @PutMapping("/restore/{id}")
    public ResponseEntity<Void> restoreProduct(
            @PathVariable UUID id,
            @RequestBody(required = false) ProductRestoreRequest request
    ) {

        productService.restoreProduct(
                id,
                request != null ? request.getReason() : null,
                request != null ? request.getRestoreOptions() : null
        );

        return ResponseEntity.noContent().build();
    }

    /* =============================
       BULK DELETE / RESTORE
       ============================= */

    @TenantManagerOnly
    @DeleteMapping("/soft/bulk")
    public ResponseEntity<Void> bulkSoftDelete(
            @RequestBody BulkActionRequest request
    ) {

        productService.bulkSoftDelete(
                request.getIds(),
                request.getReason()
        );

        return ResponseEntity.noContent().build();
    }

    @TenantManagerOnly
    @PutMapping("/restore/bulk")
    public ResponseEntity<Void> bulkRestore(
            @RequestBody BulkActionRequest request
    ) {

        productService.bulkRestore(
                request.getIds(),
                request.getReason(),
                request.getRestoreOptions()
        );

        return ResponseEntity.noContent().build();
    }

    @PlatformAdminOnly
    @DeleteMapping("/hard/bulk")
    public ResponseEntity<Void> bulkHardDelete(
            @RequestBody BulkActionRequest request
    ) throws IOException {

        productService.bulkHardDelete(
                request.getIds(),
                request.getReason()
        );

        return ResponseEntity.noContent().build();
    }

    @TenantSupervisorOnly
    @DeleteMapping("/{id}/images/{filename}")
    public ResponseEntity<Void> deleteProductImageByFilename(
            @PathVariable UUID id,
            @PathVariable String filename,
            @RequestParam(required = false, defaultValue = "true") Boolean soft
    ) throws IOException {

        productService.deleteProductImageByFilename(id, filename, soft);

        return ResponseEntity.noContent().build();
    }

    @TenantSupervisorOnly
    @DeleteMapping("/{id}/images")
    public ResponseEntity<Void> deleteAllProductImages(
            @PathVariable UUID id,
            @RequestParam(required = false, defaultValue = "true") Boolean soft,
            @RequestParam(required = false) String reason
    ) throws IOException {

        productService.deleteAllProductImages(id, soft, reason);

        return ResponseEntity.noContent().build();
    }

    @TenantSupervisorOnly
    @PutMapping("/restore/{productId}/{productImageId}")
    public ResponseEntity<Void> restoreProductImage(
            @PathVariable UUID productId,
            @PathVariable UUID productImageId,
            @RequestParam(required = false) String reason
    ) {

        productService.restoreProductImage(productId, productImageId, reason);

        return ResponseEntity.noContent().build();
    }

    /* =============================
       PRODUCT AUDITS
       ============================= */

    @TenantManagerOnly
    @GetMapping("/{id}/audits")
    public ResponseEntity<List<ProductAudit>> getProductAudits(
            @PathVariable UUID id
    ) {

        return ResponseEntity.ok(productService.getProductAudits(id));
    }

    @TenantManagerOnly
    @GetMapping("/{id}/images/audits")
    public ResponseEntity<List<ProductImageAudit>> getProductImagesAudits(
            @PathVariable UUID id
    ) {

        return ResponseEntity.ok(productService.getProductImagesAudits(id));
    }

    @PlatformAdminOnly
    @GetMapping("/images/all/audits")
    public ResponseEntity<Map<UUID, List<ProductImageAudit>>> getAllProductImagesAudits() {

        return ResponseEntity.ok(productService.getAllProductImagesAudits());
    }
}