package com.IntegrityTechnologies.business_manager.modules.stock.product.parent.controller;

import com.IntegrityTechnologies.business_manager.config.bulk.*;
import com.IntegrityTechnologies.business_manager.config.response.ApiResponse;
import com.IntegrityTechnologies.business_manager.config.response.PageWrapper;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.*;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.dto.*;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.model.ProductAudit;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.model.ProductImageAudit;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.service.ProductBulkService;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.service.ProductService;
import lombok.RequiredArgsConstructor;
import org.springframework.core.io.Resource;
import org.springframework.http.*;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import java.io.IOException;
import java.math.BigDecimal;
import java.util.List;
import java.util.Map;
import java.util.UUID;

@RestController
@RequestMapping("/api/products")
@RequiredArgsConstructor
@TenantUserOnly
public class ProductController {

    private final ProductService productService;
    private final ProductBulkService bulkService;

    /* =========================================================
       READ
       ========================================================= */

    @GetMapping
    public List<ProductDTO> getAll(@RequestParam(required = false) Boolean deleted) {
        return productService.getAllProducts(deleted);
    }

    @GetMapping("/search")
    public PageWrapper<ProductDTO> search(
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

        if (categoryId != null) categoryIds = List.of(categoryId);

        return new PageWrapper<>(
                productService.getProductsAdvanced(
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
                )
        );
    }

    @GetMapping("/{id}")
    public ProductDTO getById(@PathVariable UUID id,
                              @RequestParam(required = false) Boolean deleted) {
        return productService.getProductById(id, deleted);
    }

    @GetMapping("/sku/{sku}")
    public ProductDTO getBySku(@PathVariable String sku,
                               @RequestParam(required = false) Boolean deleted) {
        return productService.getProductBySKU(sku, deleted);
    }

    @TenantManagerOnly
    @GetMapping("/supplier/{supplierId}")
    public List<ProductDTO> getBySupplier(@PathVariable UUID supplierId,
                                          @RequestParam(required = false) Boolean deleted) {
        return productService.getProductsBySupplier(supplierId, deleted);
    }

    @GetMapping("/category/{categoryId}")
    public List<ProductDTO> getByCategory(@PathVariable Long categoryId,
                                          @RequestParam(defaultValue = "true") Boolean strict,
                                          @RequestParam(required = false) Boolean deleted) {
        return productService.getProductsByCategory(categoryId, deleted, strict);
    }

    /* =========================================================
       CREATE / UPDATE
       ========================================================= */

    @TenantSupervisorOnly
    @PostMapping(consumes = MediaType.MULTIPART_FORM_DATA_VALUE)
    public ResponseEntity<ProductDTO> create(@ModelAttribute ProductCreateDTO dto) throws IOException {
        return ResponseEntity.status(HttpStatus.CREATED)
                .body(productService.createProduct(dto));
    }

    @TenantSupervisorOnly
    @PostMapping("/full")
    public ProductDTO fullCreate(
            @RequestPart("payload") ProductFullCreateDTO dto,
            @RequestPart(value = "files", required = false) List<MultipartFile> files
    ) throws IOException {
        return productService.fullCreate(dto, files);
    }

    @TenantSupervisorOnly
    @PatchMapping("/{id}")
    public ProductDTO update(@PathVariable UUID id,
                             @RequestBody ProductUpdateDTO dto) throws IOException {
        return productService.updateProduct(id, dto);
    }

    /* =========================================================
       IMAGES
       ========================================================= */

    @GetMapping("/{id}/images")
    public List<String> getImages(@PathVariable UUID id,
                                  @RequestParam(required = false) Boolean deleted) {
        return productService.getProductImageUrls(id, deleted);
    }

    @GetMapping("/{id}/images/zip")
    public ResponseEntity<Resource> downloadImages(@PathVariable UUID id,
                                                   @RequestParam(required = false) Boolean deleted)
            throws IOException {
        return productService.downloadProductImagesZip(id, deleted);
    }

    @TenantManagerOnly
    @GetMapping("/images")
    public Map<UUID, List<String>> getAllImages() {
        return productService.getAllProductImageUrls();
    }

    @TenantManagerOnly
    @GetMapping("/images/zip")
    public ResponseEntity<Resource> downloadAllImages(
            @RequestParam(required = false) Boolean deletedProducts,
            @RequestParam(required = false) Boolean deletedImages
    ) throws IOException {
        return productService.downloadAllProductImagesZip(deletedProducts, deletedImages);
    }

    @GetMapping("/{id}/thumbnail")
    public ResponseEntity<Resource> thumbnail(@PathVariable UUID id) {
        return productService.getProductThumbnail(id);
    }

    @TenantSupervisorOnly
    @PatchMapping("/{id}/images")
    public void uploadImages(@PathVariable UUID id,
                             @RequestParam List<MultipartFile> files) throws IOException {
        productService.uploadProductImages(id, files);
    }

    @TenantSupervisorOnly
    @DeleteMapping("/{id}/images/{filename}")
    public void deleteImage(@PathVariable UUID id,
                            @PathVariable String filename,
                            @RequestParam(defaultValue = "true") Boolean soft) throws IOException {
        productService.deleteProductImageByFilename(id, filename, soft);
    }

    @TenantSupervisorOnly
    @DeleteMapping("/{id}/images")
    public void deleteAllImages(@PathVariable UUID id,
                                @RequestParam(defaultValue = "true") Boolean soft,
                                @RequestParam(required = false) String reason) throws IOException {
        productService.deleteAllProductImages(id, soft, reason);
    }

    @TenantSupervisorOnly
    @PutMapping("/{productId}/images/{imageId}/restore")
    public void restoreImage(@PathVariable UUID productId,
                             @PathVariable UUID imageId,
                             @RequestParam(required = false) String reason) {
        productService.restoreProductImage(productId, imageId, reason);
    }

    /* =========================================================
       DELETE / RESTORE
       ========================================================= */

    @TenantSupervisorOnly
    @DeleteMapping("/{id}")
    public ApiResponse softDelete(@PathVariable UUID id,
                                  @RequestParam(required = false) String reason) {
        productService.softDeleteProduct(id, reason);
        return new ApiResponse("success", "Product soft deleted");
    }

    @TenantSupervisorOnly
    @PutMapping("/{id}/restore")
    public void restore(@PathVariable UUID id,
                        @RequestBody(required = false) ProductRestoreRequest request) {
        productService.restoreProduct(
                id,
                request != null ? request.getReason() : null,
                request != null ? request.getRestoreOptions() : null
        );
    }

    @PlatformAdminOnly
    @DeleteMapping("/{id}/hard")
    public void hardDelete(@PathVariable UUID id,
                           @RequestParam(required = false) String reason) {
        productService.hardDeleteProduct(id, reason);
    }

    /* =========================================================
       BULK
       ========================================================= */

    @TenantManagerOnly
    @PostMapping("/bulk/import")
    public BulkResult<ProductDTO> importProducts(
            @RequestBody BulkRequest<ProductBulkRow> request
    ) {
        return bulkService.importProducts(request);
    }

    @TenantManagerOnly
    @PostMapping(value = "/bulk/full", consumes = MediaType.MULTIPART_FORM_DATA_VALUE)
    public BulkResult<ProductDTO> bulkFullCreate(
            @RequestPart("payload") ProductBulkFrontendRequestDTO dto,
            @RequestPart(value = "files", required = false) List<MultipartFile> files
    ) throws IOException {
        return bulkService.bulkFullCreateFrontend(dto, files);
    }

    @TenantManagerOnly
    @DeleteMapping("/bulk")
    public void bulkSoftDelete(@RequestBody BulkActionRequest request) {
        productService.bulkSoftDelete(request.getIds(), request.getReason());
    }

    @TenantManagerOnly
    @PutMapping("/bulk/restore")
    public void bulkRestore(@RequestBody BulkActionRequest request) {
        productService.bulkRestore(
                request.getIds(),
                request.getReason(),
                request.getRestoreOptions()
        );
    }

    @PlatformAdminOnly
    @DeleteMapping("/bulk/hard")
    public void bulkHardDelete(@RequestBody BulkActionRequest request) {
        productService.bulkHardDelete(request.getIds(), request.getReason());
    }

    /* =========================================================
       AUDITS
       ========================================================= */

    @TenantManagerOnly
    @GetMapping("/{id}/audits")
    public List<ProductAudit> audits(@PathVariable UUID id) {
        return productService.getProductAudits(id);
    }

    @TenantManagerOnly
    @GetMapping("/{id}/images/audits")
    public List<ProductImageAudit> imageAudits(@PathVariable UUID id) {
        return productService.getProductImagesAudits(id);
    }

    @PlatformAdminOnly
    @GetMapping("/images/audits")
    public Map<UUID, List<ProductImageAudit>> allImageAudits() {
        return productService.getAllProductImagesAudits();
    }
}