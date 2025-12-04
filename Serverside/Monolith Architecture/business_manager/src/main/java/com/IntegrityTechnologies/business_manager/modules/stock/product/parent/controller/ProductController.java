package com.IntegrityTechnologies.business_manager.modules.stock.product.parent.controller;

import com.IntegrityTechnologies.business_manager.common.PageWrapper;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.dto.ProductBulkWithFilesDTO;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.dto.ProductCreateDTO;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.dto.ProductDTO;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.dto.ProductUpdateDTO;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.model.ProductAudit;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.model.ProductImageAudit;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.service.ProductService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.core.io.FileSystemResource;
import org.springframework.core.io.Resource;
import org.springframework.http.*;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.UUID;

/**
 * ProductController — full feature set:
 * - Listing & filtering (advanced)
 * - Create / Update (multipart support, images)
 * - Barcode lookup, barcode image and PDF endpoints
 * - Soft / Restore / Hard delete
 * - Image management (upload/delete)
 */

@Tag(name = "Products")
@RestController
@RequestMapping("/api/products")
@RequiredArgsConstructor
@Slf4j
public class ProductController {

    private final ProductService productService;

    /* =============================
       BASIC READ
       ============================= */
    @PreAuthorize("""
    #deleted == false
    or 
    ( (#deleted == true  || #deleted == null) and hasAnyRole('SUPERUSER', 'ADMIN', 'MANAGER') )""")
    @GetMapping("")
    @Operation(summary = "Get all products")
    public ResponseEntity<List<ProductDTO>> getAllProducts(
            @RequestParam(required = false) Boolean deleted
    ) {
        return ResponseEntity.ok(productService.getAllProducts(deleted));
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

    @PreAuthorize("""
    #deleted == false 
    or 
    ( #deleted == true and hasAnyRole('SUPERUSER', 'ADMIN', 'MANAGER','SUPERVISOR') )""")
    @GetMapping("/{id}")
    @Operation(summary = "Get a product by ID")
    public ResponseEntity<ProductDTO> getProductById(
            @PathVariable UUID id,
            @RequestParam(required = false) Boolean deleted
    ) {
        return ResponseEntity.ok(productService.getProductById(id, deleted));
    }

    @PreAuthorize("""
    #deleted == false 
    or 
    ( #deleted == true and hasAnyRole('SUPERUSER', 'ADMIN', 'MANAGER','SUPERVISOR') )""")
    @GetMapping("/sku/{sku}")
    @Operation(summary = "Get a product by its barcode")
    public ResponseEntity<ProductDTO> getProductBySKU(
            @PathVariable String sku,
            @RequestParam(required = false) Boolean deleted
    ) {
        return ResponseEntity.ok(productService.getProductBySKU(sku, deleted));
    }

    @PreAuthorize("""
    #deleted == false 
    or 
    ( #deleted == true and hasAnyRole('SUPERUSER', 'ADMIN', 'MANAGER','SUPERVISOR') )""")
    @GetMapping("/barcode/{barcode}")
    @Operation(summary = "Get a product by its barcode")
    public ResponseEntity<ProductDTO> getProductByBarcode(
            @PathVariable String barcode,
            @RequestParam(required = false) Boolean deleted
    ) {
        return ResponseEntity.ok(productService.getProductByBarcode(barcode, deleted));
    }

    @PreAuthorize("hasAnyRole('SUPERUSER', 'ADMIN', 'MANAGER')")
    @GetMapping("/supplier/{supplierId}")
    @Operation(summary = "Get all products supplied by a specific supplier")
    public ResponseEntity<List<ProductDTO>> getProductsBySupplier(
            @PathVariable UUID supplierId,
            @RequestParam(required = false) Boolean deleted
    ) {
        return ResponseEntity.ok(productService.getProductsBySupplier(supplierId, deleted));
    }

    @PreAuthorize("""
    #deleted == false 
    or 
    ( #deleted == true and hasAnyRole('SUPERUSER', 'ADMIN', 'MANAGER','SUPERVISOR') )""")
    @GetMapping("/category/{categoryId}")
    @Operation(summary = "Get all products in a specific category")
    public ResponseEntity<List<ProductDTO>> getProductsByCategory(
            @PathVariable Long categoryId,
            @RequestParam(defaultValue = "true") Boolean strict,
            @RequestParam(required = false) Boolean deleted
    ) {
        return ResponseEntity.ok(productService.getProductsByCategory(categoryId, deleted, strict));
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
    public ResponseEntity<Resource> getBarcodePdfSheet(@RequestBody List<UUID> ids) throws IOException {
        File pdf = productService.getBarcodePdfSheet(ids);
        Resource res = new FileSystemResource(pdf);
        return ResponseEntity.ok()
                .contentType(MediaType.APPLICATION_PDF)
                .header(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=barcode-sheet.pdf")
                .body(res);
    }







    /* =============================
   IMAGE DOWNLOAD ENDPOINTS
   ============================= */

    /** Return all image URLs for a single product */
    @PreAuthorize("""
    #deleted == false 
    or 
    ( #deleted == true and hasAnyRole('SUPERUSER', 'ADMIN') )""")
    @GetMapping("/{id}/images")
    @Operation(summary = "Get image URLs for a product")
    public ResponseEntity<List<String>> getProductImageUrls(
            @PathVariable UUID id,
            @RequestParam(required = false) Boolean deleted
    ) {
        return ResponseEntity.ok(productService.getProductImageUrls(id, deleted));
    }

    /** Return a ZIP of all image files for a single product */
    @PreAuthorize("""
    #deleted == false 
    or 
    ( #deleted == true and hasAnyRole('SUPERUSER', 'ADMIN') )""")
    @GetMapping("/{id}/images/zip")
    @Operation(summary = "Download all images of a product as a ZIP")
    public ResponseEntity<Resource> downloadProductImagesZip(
            @PathVariable UUID id,
            @RequestParam(required = false) Boolean deleted
    ) throws IOException {
        File zip = productService.zipProductImages(id, deleted);
        Resource res = new FileSystemResource(zip);
        return ResponseEntity.ok()
                .contentType(MediaType.APPLICATION_OCTET_STREAM)
                .header(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=product-" + id + "-images.zip")
                .body(res);
    }

    @PreAuthorize("""
    #deleted == false 
    or 
    ( #deleted == true and hasAnyRole('SUPERUSER', 'ADMIN') )""")
    @GetMapping("/images/all/zip")
    @Operation(summary = "Download all product images from the system as a ZIP")
    public ResponseEntity<Resource> downloadAllProductImagesZip(
            @RequestParam(required = false) Boolean deletedProducts,
            @RequestParam(required = false) Boolean deletedImages
    ) throws IOException {
        File zip = productService.zipAllProductImages(deletedProducts, deletedImages);
        Resource res = new FileSystemResource(zip);
        return ResponseEntity.ok()
                .contentType(MediaType.APPLICATION_OCTET_STREAM)
                .header(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=all-product-images.zip")
                .body(res);
    }

    @PreAuthorize("""
    #deleted == false 
    or 
    ( #deleted == true and hasAnyRole('SUPERUSER', 'ADMIN') )""")
    @GetMapping("/images/all")
    @Operation(summary = "Get URLs for all product images in the system")
    public ResponseEntity<Map<UUID, List<String>>> getAllProductImageUrls() {
        return ResponseEntity.ok(productService.getAllProductImageUrls());
    }










    /* =============================
       CREATE / UPDATE
       ============================= */

    @PostMapping(value="/create", consumes = MediaType.MULTIPART_FORM_DATA_VALUE)
    public ResponseEntity<ProductDTO> createProduct(
            @ModelAttribute("product") ProductCreateDTO dto
    ) throws IOException {
        ProductDTO created = productService.createProduct(dto);
        return ResponseEntity.status(HttpStatus.CREATED).body(created);
    }

    @PostMapping(
            value = "/create/bulk",
            consumes = MediaType.MULTIPART_FORM_DATA_VALUE,
            produces = MediaType.APPLICATION_JSON_VALUE
    )
    public ResponseEntity<List<ProductDTO>> createProductInBulk(
            @ModelAttribute ProductBulkWithFilesDTO bulkDTO,
            Authentication authentication
    ) throws IOException {
        String creatorUsername = (authentication != null && authentication.getPrincipal() instanceof UserDetails ud)
                ? ud.getUsername()
                : null;
        List<ProductDTO> savedProductDTOs = new ArrayList<>();


        for (ProductCreateDTO productCreateDTO : bulkDTO.getProducts()) {
            savedProductDTOs.add(productService.createProduct(productCreateDTO));
        }
        return ResponseEntity.status(HttpStatus.CREATED).body(savedProductDTOs);
    }

    @PreAuthorize("hasAnyRole('SUPERUSER', 'ADMIN', 'MANAGER', 'SUPERVISOR')")
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
    @PreAuthorize("hasAnyRole('SUPERUSER', 'ADMIN', 'MANAGER', 'SUPERVISOR')")
    @PatchMapping("/{id}/images")
    @Operation(summary = "Upload images for a product (appends to existing)")
    public ResponseEntity<Void> uploadImages(@PathVariable UUID id,
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
    public ResponseEntity<Void> hardDeleteProduct(@PathVariable UUID id) throws IOException {
        productService.hardDeleteProduct(id);
        return ResponseEntity.noContent().build();
    }

    @PreAuthorize("hasAnyRole('SUPERUSER', 'ADMIN', 'MANAGER', 'SUPERVISOR')")
    @DeleteMapping("/soft/{id}")
    @Operation(summary = "Soft delete a product (mark as deleted)")
    public ResponseEntity<?> softDeleteProduct(@PathVariable UUID id) {
        return productService.softDeleteProduct(id);
    }

    @PreAuthorize("hasAnyRole('SUPERUSER', 'ADMIN', 'MANAGER', 'SUPERVISOR')")
    @PutMapping("/restore/{id}")
    @Operation(summary = "Restore a soft-deleted product")
    public ResponseEntity<Void> restoreProduct(@PathVariable UUID id) {
        productService.restoreProduct(id);
        return ResponseEntity.noContent().build();
    }

    /* =============================
   BULK DELETE & RESTORE
   ============================= */

    @PreAuthorize("hasAnyRole('SUPERUSER','ADMIN','MANAGER')")
    @DeleteMapping("/soft/bulk")
    @Operation(summary = "Soft delete products in bulk")
    public ResponseEntity<Void> bulkSoftDelete(@RequestBody List<UUID> ids) {
        productService.bulkSoftDelete(ids);
        return ResponseEntity.noContent().build();
    }

    @PreAuthorize("hasAnyRole('SUPERUSER','ADMIN','MANAGER')")
    @PutMapping("/restore/bulk")
    @Operation(summary = "Restore soft-deleted products in bulk")
    public ResponseEntity<Void> bulkRestore(@RequestBody List<UUID> ids) {
        productService.bulkRestore(ids);
        return ResponseEntity.noContent().build();
    }

    @PreAuthorize("hasRole('SUPERUSER')")
    @DeleteMapping("/hard/bulk")
    @Operation(summary = "Hard delete products in bulk")
    public ResponseEntity<Void> bulkHardDelete(@RequestBody List<UUID> ids) throws IOException {
        productService.bulkHardDelete(ids);
        return ResponseEntity.noContent().build();
    }

    @PreAuthorize("hasRole('SUPERUSER')")
    @DeleteMapping("/{id}/images/{filename}")
    @Operation(summary = "Delete a specific product image by filename")
    public ResponseEntity<Void> deleteProductImageByFilename(
            @PathVariable UUID id,
            @PathVariable String filename
    ) throws IOException {
        productService.deleteProductImageByFilename(id, filename);
        return ResponseEntity.noContent().build();
    }

    @PreAuthorize("hasRole('SUPERUSER')")
    @DeleteMapping("/{id}/images")
    @Operation(summary = "Delete all images for a product (and remove upload directory)")
    public ResponseEntity<Void> deleteAllProductImages(@PathVariable UUID id) throws IOException {
        productService.deleteAllProductImages(id);
        return ResponseEntity.noContent().build();
    }











    /* =============================
   PRODUCT AUDITS
   ============================= */

    @PreAuthorize("hasAnyRole('SUPERUSER', 'ADMIN', 'MANAGER')")
    @GetMapping("/{id}/audits")
    @Operation(summary = "Get audit history for a single product")
    public ResponseEntity<List<ProductAudit>> getProductAudits(@PathVariable UUID id) {
        return ResponseEntity.ok(productService.getProductAudits(id));
    }

    @PreAuthorize("hasAnyRole('SUPERUSER', 'ADMIN', 'MANAGER')")
    @GetMapping("/{id}/images/audits")
    @Operation(summary = "Get audit history for all images of a single product")
    public ResponseEntity<List<ProductImageAudit>> getProductImagesAudits(@PathVariable UUID id) {
        return ResponseEntity.ok(productService.getProductImagesAudits(id));
    }

    @PreAuthorize("hasAnyRole('SUPERUSER')")
    @GetMapping("/images/all/audits")
    @Operation(summary = "Get audit history for all product images in the system")
    public ResponseEntity<Map<UUID, List<ProductImageAudit>>> getAllProductImagesAudits() {
        return ResponseEntity.ok(productService.getAllProductImagesAudits());
    }
}