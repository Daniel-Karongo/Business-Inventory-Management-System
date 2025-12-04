package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.controller;

import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.dto.*;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.service.ProductVariantService;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.UUID;

@RestController
@RequestMapping("/api/product-variants")
@RequiredArgsConstructor
@Tag(name = "Product Variants")
public class ProductVariantController {

    private final ProductVariantService service;

    @PreAuthorize("hasAnyRole('SUPERUSER','ADMIN','MANAGER')")
    @PostMapping
    public ResponseEntity<ProductVariantDTO> create(@RequestBody ProductVariantCreateDTO dto) {
        return ResponseEntity.ok(service.createVariant(dto));
    }

    @GetMapping("/{id}")
    public ResponseEntity<ProductVariantDTO> find(@PathVariable UUID id) {
        return ResponseEntity.ok(service.getVariant(id));
    }

    @GetMapping("/product/{productId}")
    public ResponseEntity<List<ProductVariantDTO>> findForProduct(@PathVariable UUID productId) {
        return ResponseEntity.ok(service.getVariantsForProduct(productId));
    }

    @PreAuthorize("hasAnyRole('SUPERUSER','ADMIN','MANAGER')")
    @PutMapping("/{id}")
    public ResponseEntity<ProductVariantDTO> update(@PathVariable UUID id,
                                                    @RequestBody ProductVariantUpdateDTO dto) {
        return ResponseEntity.ok(service.updateVariant(id, dto));
    }

    @PreAuthorize("hasAnyRole('SUPERUSER','ADMIN')")
    @DeleteMapping("/{id}")
    public ResponseEntity<Void> delete(@PathVariable UUID id) {
        service.deleteVariant(id);
        return ResponseEntity.noContent().build();
    }
}
