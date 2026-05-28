package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.packaging.controller;

import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.packaging.dto.CreatePackagingRequest;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.packaging.dto.PackagingDTO;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.packaging.dto.UpdatePackagingRequest;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.packaging.model.ProductVariantPackaging;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.packaging.service.ProductVariantPackagingService;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.UUID;

@RestController
@RequestMapping("/api/product-variants/packaging")
@RequiredArgsConstructor
public class ProductVariantPackagingController {

    private final ProductVariantPackagingService service;

    @PostMapping
    public PackagingDTO create(
            @RequestBody CreatePackagingRequest req
    ) {

        return map(
                service.createPackaging(
                        req.getBranchId(),
                        req.getVariantId(),
                        req.getName(),
                        req.getUnitsPerPackaging()
                )
        );
    }

    @GetMapping("/{variantId}")
    public List<PackagingDTO> get(
            @PathVariable UUID variantId
    ) {

        return service.getPackagings(variantId)
                .stream()
                .map(this::map)
                .toList();
    }

    @GetMapping("/{variantId}/base")
    public PackagingDTO getBase(
            @PathVariable UUID variantId
    ) {

        return map(
                service.getBasePackaging(variantId)
        );
    }

    @PutMapping("/{id}")
    public PackagingDTO update(
            @PathVariable UUID id,
            @RequestBody UpdatePackagingRequest req
    ) {

        return map(
                service.updatePackaging(
                        req.getBranchId(),
                        id,
                        req.getName(),
                        req.getUnitsPerPackaging()
                )
        );
    }

    @DeleteMapping("/{id}")
    public void delete(
            @PathVariable UUID id,
            @RequestParam UUID branchId
    ) {

        service.deletePackaging(branchId, id);
    }

    private PackagingDTO map(
            ProductVariantPackaging p
    ) {

        return PackagingDTO.builder()
                .packagingId(p.getId())
                .variantId(p.getProductVariant().getId())
                .name(p.getName())
                .unitsPerPackaging(p.getUnitsPerPackaging())
                .isBaseUnit(p.getIsBaseUnit())
                .build();
    }
}