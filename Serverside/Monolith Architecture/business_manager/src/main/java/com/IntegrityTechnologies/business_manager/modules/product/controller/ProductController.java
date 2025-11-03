package com.IntegrityTechnologies.business_manager.modules.product.controller;

import com.IntegrityTechnologies.business_manager.common.PageWrapper;
import com.IntegrityTechnologies.business_manager.modules.product.dto.ProductDTO;
import com.IntegrityTechnologies.business_manager.modules.product.service.ProductService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.core.io.FileSystemResource;
import org.springframework.core.io.Resource;
import org.springframework.http.*;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import java.io.File;
import java.io.IOException;
import java.util.List;

/**
 * ProductController — full feature set:
 * - Listing & filtering (advanced)
 * - Create / Update (multipart support, images)
 * - Barcode lookup, barcode image and PDF endpoints
 * - Soft / Restore / Hard delete
 * - Image management (upload/delete)
 */
@RestController
@RequestMapping("/api/products")
@RequiredArgsConstructor
@Slf4j
@Tag(name = "Products", description = "Endpoints for managing products, barcodes, and images")
public class ProductController {

    private final ProductService productService;

    /* =============================
       BASIC READ
       ============================= */
    @GetMapping("/active")
    @Operation(summary = "Get all products")
    public ResponseEntity<List<ProductDTO>> getAllActiveProducts() {
        return ResponseEntity.ok(productService.getAllActiveProducts());
    }

    @PreAuthorize("hasRole('SUPERUSER')")
    @GetMapping("/deleted")
    @Operation(summary = "Get all products")
    public ResponseEntity<List<ProductDTO>> getAllDeletedProducts() {
        return ResponseEntity.ok(productService.getAllDeletedProducts());
    }

    @PreAuthorize("hasRole('SUPERUSER')")
    @GetMapping("/all")
    @Operation(summary = "Get all products")
    public ResponseEntity<List<ProductDTO>> getAllActiveAndDeletedProducts() {
        return ResponseEntity.ok(productService.getAllActiveAndDeletedProducts());
    }

    @PreAuthorize("hasAnyRole('SUPERUSER', 'ADMIN', 'MANAGER', 'SUPERVISOR')")
    @GetMapping("/advanced")
    @Operation(summary = "Advanced product search and filtering")
    public ResponseEntity<PageWrapper<ProductDTO>> getProductsAdvanced(
            @RequestParam(required = false) List<Long> categoryIds,
            @RequestParam(required = false) String name,
            @RequestParam(required = false) String description,
            @RequestParam(required = false) Double minPrice,
            @RequestParam(required = false) Double maxPrice,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "id") String sortBy,
            @RequestParam(defaultValue = "asc") String direction,
            @RequestParam(defaultValue = "false") boolean includeDeleted
    ) {
        var result = productService.getProductsAdvanced(
                categoryIds, name, description,
                minPrice != null ? new java.math.BigDecimal(minPrice) : null,
                maxPrice != null ? new java.math.BigDecimal(maxPrice) : null,
                page, size, sortBy, direction, includeDeleted
        );
        return ResponseEntity.ok(new PageWrapper<>(result));
    }

    @GetMapping("/{id}")
    @Operation(summary = "Get a product by ID")
    public ResponseEntity<ProductDTO> getProductById(@PathVariable Long id,
                                                     @RequestParam(defaultValue = "false") boolean includeDeleted) {
        return ResponseEntity.ok(productService.getProductById(id, includeDeleted));
    }

    @GetMapping("/barcode/{barcode}")
    @Operation(summary = "Get a product by its barcode")
    public ResponseEntity<ProductDTO> getProductByBarcode(@PathVariable String barcode) {
        return ResponseEntity.ok(productService.getProductByBarcode(barcode));
    }

    /* =============================
       BARCODE IMAGE + PDF
       ============================= */
    @GetMapping("/barcode/image/{barcode}")
    @Operation(summary = "Download barcode image for a product")
    public ResponseEntity<Resource> getBarcodeImage(@PathVariable String barcode) {
        File imageFile = productService.getBarcodeImageFile(barcode);
        if (imageFile == null || !imageFile.exists()) return ResponseEntity.notFound().build();

        Resource resource = new FileSystemResource(imageFile);
        return ResponseEntity.ok()
                .contentType(MediaType.IMAGE_PNG)
                .header(HttpHeaders.CONTENT_DISPOSITION, "inline; filename=" + imageFile.getName())
                .body(resource);
    }

    @GetMapping("/barcode/pdf/{barcode}")
    @Operation(summary = "Download a polished single-barcode PDF label")
    public ResponseEntity<Resource> getBarcodePdfSingle(@PathVariable String barcode) throws IOException {
        File pdf = productService.getBarcodePdfSingle(barcode);
        Resource res = new FileSystemResource(pdf);
        return ResponseEntity.ok()
                .contentType(MediaType.APPLICATION_PDF)
                .header(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=barcode-" + barcode + ".pdf")
                .body(res);
    }

    @PreAuthorize("hasAnyRole('SUPERUSER', 'ADMIN', 'MANAGER', 'SUPERVISOR')")
    @PostMapping("/barcode/pdf/sheet")
    @Operation(summary = "Download a polished barcode sheet PDF for multiple products")
    public ResponseEntity<Resource> getBarcodePdfSheet(@RequestBody List<Long> ids) throws IOException {
        File pdf = productService.getBarcodePdfSheet(ids);
        Resource res = new FileSystemResource(pdf);
        return ResponseEntity.ok()
                .contentType(MediaType.APPLICATION_PDF)
                .header(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=barcode-sheet.pdf")
                .body(res);
    }

    /* =============================
       CREATE / UPDATE
       ============================= */
    @PreAuthorize("hasAnyRole('SUPERUSER', 'ADMIN', 'MANAGER', 'SUPERVISOR')")
    @PostMapping(consumes = MediaType.MULTIPART_FORM_DATA_VALUE)
    @Operation(summary = "Create a new product with optional images")
    public ResponseEntity<ProductDTO> createProduct(
            @RequestPart("product") ProductDTO dto,
            @RequestPart(value = "images", required = false) List<MultipartFile> imageFiles
    ) throws IOException {
        ProductDTO created = productService.createProduct(dto, imageFiles);
        return ResponseEntity.status(HttpStatus.CREATED).body(created);
    }

    @PreAuthorize("hasAnyRole('SUPERUSER', 'ADMIN', 'MANAGER', 'SUPERVISOR')")
    @PutMapping(value = "/{id}", consumes = MediaType.MULTIPART_FORM_DATA_VALUE)
    @Operation(summary = "Update an existing product (partial fields allowed)")
    public ResponseEntity<ProductDTO> updateProduct(
            @PathVariable Long id,
            @RequestPart("product") ProductDTO dto,
            @RequestPart(value = "images", required = false) List<MultipartFile> imageFiles
    ) throws IOException {
        return ResponseEntity.ok(productService.updateProduct(id, dto, imageFiles));
    }

    /* =============================
   IMAGE MANAGEMENT
   ============================= */
    @PreAuthorize("hasAnyRole('SUPERUSER', 'ADMIN', 'MANAGER', 'SUPERVISOR')")
    @PostMapping("/{id}/images")
    @Operation(summary = "Upload images for a product (appends to existing)")
    public ResponseEntity<Void> uploadImages(@PathVariable Long id,
                                             @RequestParam("files") List<MultipartFile> files) throws IOException {
        productService.uploadProductImages(id, files);
        return ResponseEntity.noContent().build();
    }


    /* =============================
       DELETE / RESTORE
       ============================= */

    @PreAuthorize("hasRole('SUPERUSER')")
    @DeleteMapping("/hard/{id}")
    @Operation(summary = "Permanently delete a product and its images")
    public ResponseEntity<Void> hardDeleteProduct(@PathVariable Long id) throws IOException {
        productService.hardDeleteProduct(id);
        return ResponseEntity.noContent().build();
    }

    @PreAuthorize("hasAnyRole('SUPERUSER', 'ADMIN', 'MANAGER')")
    @DeleteMapping("/soft/{id}")
    @Operation(summary = "Soft delete a product (mark as deleted)")
    public ResponseEntity<Void> softDeleteProduct(@PathVariable Long id) {
        productService.softDeleteProduct(id);
        return ResponseEntity.noContent().build();
    }

    @PreAuthorize("hasAnyRole('SUPERUSER', 'ADMIN', 'MANAGER')")
    @PutMapping("/restore/{id}")
    @Operation(summary = "Restore a soft-deleted product")
    public ResponseEntity<Void> restoreProduct(@PathVariable Long id) {
        productService.restoreProduct(id);
        return ResponseEntity.noContent().build();
    }

    @PreAuthorize("hasRole('SUPERUSER')")
    @DeleteMapping("/{id}/images/{filename}")
    @Operation(summary = "Delete a specific product image by filename")
    public ResponseEntity<Void> deleteProductImageByFilename(
            @PathVariable Long id,
            @PathVariable String filename
    ) throws IOException {
        productService.deleteProductImageByFilename(id, filename);
        return ResponseEntity.noContent().build();
    }

    @PreAuthorize("hasRole('SUPERUSER')")
    @DeleteMapping("/{id}/images")
    @Operation(summary = "Delete all images for a product (and remove upload directory)")
    public ResponseEntity<Void> deleteAllProductImages(@PathVariable Long id) throws IOException {
        productService.deleteAllProductImages(id);
        return ResponseEntity.noContent().build();
    }
}