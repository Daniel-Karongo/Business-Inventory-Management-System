package com.IntegrityTechnologies.business_manager.modules.product.controller;

import com.IntegrityTechnologies.business_manager.modules.product.dto.ProductDTO;
import com.IntegrityTechnologies.business_manager.modules.product.service.ProductService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.core.io.Resource;
import org.springframework.data.domain.Page;
import org.springframework.http.*;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;
import jakarta.validation.Valid;

import java.io.IOException;
import java.math.BigDecimal;
import java.nio.file.Files;
import java.util.List;

@Tag(name = "Products", description = "Product management, filtering, and image handling")
@RestController
@RequestMapping("/api/products")
@RequiredArgsConstructor
public class ProductController {

    private final ProductService productService;

    @PreAuthorize("hasRole('ADMIN')")
    @PostMapping(consumes = MediaType.MULTIPART_FORM_DATA_VALUE)
    public ResponseEntity<ProductDTO> saveProduct(@Valid @ModelAttribute ProductDTO productDTO) throws IOException {
        return ResponseEntity.ok(productService.saveProduct(productDTO));
    }

    @Operation(summary = "Get all products (optionally include deleted ones)")
    @GetMapping("/advanced")
    public ResponseEntity<Page<ProductDTO>> getProductsAdvanced(
            @RequestParam(required = false) List<Long> categoryIds,
            @RequestParam(required = false) String name,
            @RequestParam(required = false) String description,
            @RequestParam(required = false) BigDecimal minPrice,
            @RequestParam(required = false) BigDecimal maxPrice,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "id") String sortBy,
            @RequestParam(defaultValue = "desc") String direction,
            @RequestParam(defaultValue = "false") boolean includeDeleted
    ) {
        return ResponseEntity.ok(productService.getProductsAdvanced(
                categoryIds, name, description, minPrice, maxPrice, page, size, sortBy, direction, includeDeleted));
    }

    /**
     * Get only active (non-deleted) products list (non-paginated, convenience)
     * ADMIN may want to list deleted via /deleted endpoint
     */
    @GetMapping("/active")
    public ResponseEntity<List<ProductDTO>> getActiveProducts() {
        return ResponseEntity.ok(productService.getActiveProducts());
    }

    @PreAuthorize("hasRole('ADMIN')")
    @GetMapping("/deleted")
    public ResponseEntity<List<ProductDTO>> getDeletedProducts() {
        return ResponseEntity.ok(productService.getDeletedProducts());
    }

    @GetMapping("/{id}")
    public ResponseEntity<ProductDTO> getProduct(
            @PathVariable Long id,
            @RequestParam(defaultValue = "false") boolean includeDeleted
    ) {
        return ResponseEntity.ok(productService.getProduct(id, includeDeleted));
    }

    @PreAuthorize("hasRole('ADMIN')")
    @DeleteMapping("/soft/{id}")
    public ResponseEntity<Void> softDeleteProduct(@PathVariable Long id) {
        productService.softDeleteProduct(id);
        return ResponseEntity.noContent().build();
    }

    @PreAuthorize("hasRole('ADMIN')")
    @PutMapping("/restore/{id}")
    public ResponseEntity<Void> restoreProduct(@PathVariable Long id) {
        productService.restoreProduct(id);
        return ResponseEntity.noContent().build();
    }

    @PreAuthorize("hasRole('SUPERUSER')")
    @DeleteMapping("/permanent/{id}")
    public ResponseEntity<Void> permanentDeleteProduct(@PathVariable Long id) throws IOException {
        productService.permanentDeleteProduct(id);
        return ResponseEntity.noContent().build();
    }

    @Operation(summary = "Serve product image")
    @GetMapping("/image/{filename:.+}")
    public ResponseEntity<Resource> getProductImage(@PathVariable String filename) throws IOException {
        Resource resource = productService.loadProductImage(filename);
        String contentType = Files.probeContentType(resource.getFile().toPath());
        return ResponseEntity.ok()
                .contentType(MediaType.parseMediaType(contentType != null ? contentType : "application/octet-stream"))
                .body(resource);
    }
}