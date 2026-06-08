package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.controller;

import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.dto.CreatePriceRequest;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.dto.ProductPriceDTO;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.dto.UpdatePriceRequest;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.model.ProductPrice;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.service.ProductPriceService;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.UUID;

@RestController
@RequestMapping("/api/product-variants/pricing")
@RequiredArgsConstructor
public class PricingController {

    private final ProductPriceService service;

    @GetMapping("/variant/{variantId}")
    public List<ProductPriceDTO> getVariantPrices(
            @PathVariable UUID variantId,
            @RequestParam UUID branchId
    ) {

        return service
                .getPricesForVariant(branchId, variantId)
                .stream()
                .map(this::map)
                .toList();
    }

    @PostMapping
    public ProductPriceDTO create(
            @RequestBody CreatePriceRequest req
    ) {

        return map(
                service.createPrice(
                        req.getBranchId(),
                        req.getVariantId(),
                        req.getPackagingId(),
                        req.getPrice(),
                        req.getMinQuantity()
                )
        );
    }

    @PutMapping("/{id}")
    public ProductPriceDTO update(
            @PathVariable UUID id,
            @RequestBody UpdatePriceRequest req
    ) {

        return map(
                service.updatePrice(
                        req.getBranchId(),
                        id,
                        req.getPrice()
                )
        );
    }

    @DeleteMapping("/{id}")
    public void delete(
            @PathVariable UUID id,
            @RequestParam UUID branchId
    ) {

        service.deletePrice(branchId, id);
    }

    private ProductPriceDTO map(
            ProductPrice p
    ) {

        return ProductPriceDTO.builder()
                .id(p.getId())
                .variantId(
                        p.getProductVariant().getId()
                )
                .packagingId(
                        p.getPackaging().getId()
                )
                .packagingName(
                        p.getPackaging().getName()
                )
                .unitsPerPackaging(
                        p.getPackaging().getUnitsPerPackaging()
                )
                .price(p.getPrice())
                .minQuantity(p.getMinQuantity())
                .deleted(p.getDeleted())
                .build();
    }
}