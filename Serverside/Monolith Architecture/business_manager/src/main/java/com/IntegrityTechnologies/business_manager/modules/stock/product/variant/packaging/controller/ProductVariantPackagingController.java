package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.packaging.controller;

import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.packaging.dto.CreatePackagingRequest;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.packaging.dto.UpdatePackagingRequest;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.packaging.model.ProductVariantPackaging;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.packaging.service.ProductVariantPackagingService;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.UUID;

@RestController
@RequestMapping("/api/packaging")
@RequiredArgsConstructor
public class ProductVariantPackagingController {

    private final ProductVariantPackagingService service;

    @PostMapping
    public ProductVariantPackaging create(@RequestBody CreatePackagingRequest req) {
        return service.createPackaging(
                req.getVariantId(),
                req.getName(),
                req.getUnitsPerPackaging()
        );
    }

    @GetMapping("/{variantId}")
    public List<ProductVariantPackaging> get(@PathVariable UUID variantId) {
        return service.getPackagings(variantId);
    }

    @PutMapping("/{id}")
    public ProductVariantPackaging update(
            @PathVariable UUID id,
            @RequestBody UpdatePackagingRequest req
    ) {
        return service.updatePackaging(id, req.getName(), req.getUnitsPerPackaging());
    }

    @DeleteMapping("/{id}")
    public void delete(@PathVariable UUID id) {
        service.deletePackaging(id);
    }
}