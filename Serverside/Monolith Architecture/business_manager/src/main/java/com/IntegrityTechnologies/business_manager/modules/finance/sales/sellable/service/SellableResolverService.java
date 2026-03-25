package com.IntegrityTechnologies.business_manager.modules.finance.sales.sellable.service;

import com.IntegrityTechnologies.business_manager.modules.finance.sales.sellable.dto.ResolvedSellable;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.sellable.dto.SellableResolveResponse;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.service.InventoryService;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.model.PricingContext;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.model.PricingResult;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.service.PricingEngineService;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class SellableResolverService {

    private final PricingEngineService pricingEngine;
    private final InventoryService inventoryService;


    public ResolvedSellable resolve(
            UUID variantId,
            UUID packagingId,
            long quantity,
            UUID branchId,
            UUID customerId,
            UUID customerGroupId,
            long unitsPerPackaging
    ) {

        long baseUnits = quantity * unitsPerPackaging;
        BigDecimal unitCost =
                inventoryService.getAverageCost(variantId, branchId);
        // =========================
        // PRICING
        // =========================
        PricingResult pricing = pricingEngine.resolve(
                PricingContext.builder()
                        .tenantId(TenantContext.getTenantId())
                        .branchId(branchId)
                        .productVariantId(variantId)
                        .packagingId(packagingId)
                        .quantity(baseUnits)
                        .cost(unitCost)
                        .customerId(customerId)
                        .customerGroupId(customerGroupId)
                        .pricingTime(LocalDateTime.now())
                        .build()
        );

        // =========================
        // STOCK
        // =========================
        long availableBase = inventoryService.availableQuantity(
                variantId,
                branchId
        );

        long availablePackaging =
                unitsPerPackaging > 0
                        ? availableBase / unitsPerPackaging
                        : 0;

        return ResolvedSellable.builder()
                .unitPrice(pricing.getFinalPrice())
                .totalPrice(
                        pricing.getFinalPrice()
                                .multiply(BigDecimal.valueOf(quantity))
                )
                .availableQuantity(availablePackaging)
                .baseUnits(baseUnits)
                .adjustments(pricing.getAdjustments())
                .build();
    }

    public SellableResolveResponse resolveFull(
            UUID variantId,
            UUID packagingId,
            long quantity,
            UUID branchId,
            UUID customerId,
            UUID customerGroupId,
            long unitsPerPackaging,
            List<UUID> batchIds
    ) {

        long baseUnits = quantity * unitsPerPackaging;

        // =========================
        // STOCK VALIDATION
        // =========================
        long available = inventoryService.availableQuantity(
                variantId,
                branchId
        );

        if (available < baseUnits) {
            throw new IllegalStateException("Insufficient stock");
        }

        // =========================
        // ALLOCATION
        // =========================
        Map<String, Object> allocation =
                inventoryService.previewAllocation(
                        variantId,
                        branchId,
                        baseUnits,
                        batchIds
                );

        BigDecimal totalCost = (BigDecimal) allocation.get("totalCost");
        BigDecimal unitCost =
                totalCost.divide(BigDecimal.valueOf(baseUnits), 6, RoundingMode.HALF_UP);

        // =========================
        // PRICING
        // =========================
        PricingResult pricing = pricingEngine.resolve(
                PricingContext.builder()
                        .tenantId(TenantContext.getTenantId())
                        .branchId(branchId)
                        .productVariantId(variantId)
                        .packagingId(packagingId)
                        .quantity(baseUnits)
                        .cost(unitCost)
                        .customerId(customerId)
                        .customerGroupId(customerGroupId)
                        .pricingTime(LocalDateTime.now())
                        .build()
        );

        return SellableResolveResponse.builder()
                .productVariantId(variantId)
                .packagingId(packagingId)
                .requestedQuantity(quantity)
                .baseUnits(baseUnits)
                .unitPrice(pricing.getFinalPrice())
                .totalPrice(
                        pricing.getFinalPrice()
                                .multiply(BigDecimal.valueOf(quantity))
                )
                .availableStock(available)
                .totalCost(totalCost)
                .batchAllocations((List<Map<String, Object>>) allocation.get("allocations"))
                .adjustments(pricing.getAdjustments())
                .build();
    }
}
