package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.controller;

import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.dto.CreatePriceRequest;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.dto.UpdatePriceRequest;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.model.*;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.service.PricingEngineService;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.service.ProductPriceService;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

import java.time.LocalDateTime;
import java.util.UUID;

@RestController
@RequestMapping("/api/pricing")
@RequiredArgsConstructor
public class PricingController {

    private final PricingEngineService pricingEngine;
    private final ProductPriceService service;

    @PostMapping("/resolve")
    public PricingResult resolve(@RequestBody PricingContext ctx) {
        if (ctx.getPricingTime() == null) {
            ctx.setPricingTime(LocalDateTime.now());
        }
        return pricingEngine.resolve(ctx);
    }

    @PostMapping
    public ProductPrice create(@RequestBody CreatePriceRequest req) {
        return service.createPrice(
                req.getVariantId(),
                req.getPackagingId(),
                req.getPrice(),
                req.getMinQuantity()
        );
    }

    @PutMapping("/{id}")
    public ProductPrice update(
            @PathVariable UUID id,
            @RequestBody UpdatePriceRequest req
    ) {
        return service.updatePrice(id, req.getPrice());
    }

    @DeleteMapping("/{id}")
    public void delete(@PathVariable UUID id) {
        service.deletePrice(id);
    }
}