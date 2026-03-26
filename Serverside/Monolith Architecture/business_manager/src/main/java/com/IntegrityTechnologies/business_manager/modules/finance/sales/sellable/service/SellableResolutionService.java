package com.IntegrityTechnologies.business_manager.modules.finance.sales.sellable.service;

import com.IntegrityTechnologies.business_manager.modules.finance.sales.sellable.domain.*;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.sellable.dto.AllocationResult;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.service.TaxSystemStateService;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.service.InventoryService;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.packaging.model.ProductVariantPackaging;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.packaging.service.ProductVariantPackagingService;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.model.PricingContext;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.model.PricingResult;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.service.PricingEngineService;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDateTime;
import java.util.*;

@Service
@RequiredArgsConstructor
public class SellableResolutionService {

    private final ProductVariantPackagingService packagingService;
    private final PricingEngineService pricingEngine;
    private final InventoryService inventoryService;
    private final TaxSystemStateService taxSystemStateService;
    private final ObjectMapper objectMapper;

    public SellableSnapshot resolve(SellableContext ctx) {

        // =========================
        // 1. PACKAGING
        // =========================
        ProductVariantPackaging packaging =
                (ctx.getPackagingId() != null)
                        ? packagingService.getPackagings(ctx.getProductVariantId())
                        .stream()
                        .filter(p -> p.getId().equals(ctx.getPackagingId()))
                        .findFirst()
                        .orElseThrow(() -> new IllegalArgumentException("Invalid packaging"))
                        : packagingService.getBasePackaging(ctx.getProductVariantId());

        long baseUnits = ctx.getQuantity() * packaging.getUnitsPerPackaging();

        // =========================
        // 2. STOCK
        // =========================
        long available = inventoryService.availableQuantity(
                ctx.getProductVariantId(),
                ctx.getBranchId()
        );

        boolean sufficient = available >= baseUnits;

        List<String> warnings = new ArrayList<>();

        if (!sufficient) {
            if (ctx.getMode() == ResolutionMode.FINAL_STRICT) {
                throw new IllegalStateException("Insufficient stock");
            } else {
                warnings.add("INSUFFICIENT_STOCK");
            }
        }

        // =========================
        // 3. COST STRATEGY
        // =========================
        BigDecimal totalCost;
        BigDecimal unitCost;
        AllocationResult allocation = null;

        if (ctx.getMode() == ResolutionMode.UI_FAST) {

            unitCost = inventoryService.getAverageCost(
                    ctx.getProductVariantId(),
                    ctx.getBranchId()
            );

            totalCost = unitCost.multiply(BigDecimal.valueOf(baseUnits));

        } else {

            allocation = inventoryService.previewAllocation(
                    ctx.getProductVariantId(),
                    ctx.getBranchId(),
                    baseUnits,
                    ctx.getBatchIds()
            );

            totalCost = allocation.getTotalCost();

            unitCost = baseUnits == 0
                    ? BigDecimal.ZERO
                    : totalCost.divide(
                    BigDecimal.valueOf(baseUnits),
                    6,
                    RoundingMode.HALF_UP
            );
        }

        // =========================
        // 4. PRICING
        // =========================
        PricingResult pricing = pricingEngine.resolve(
                PricingContext.builder()
                        .tenantId(ctx.getTenantId())
                        .branchId(ctx.getBranchId())
                        .productVariantId(ctx.getProductVariantId())
                        .packagingId(packaging.getId())
                        .quantity(baseUnits)
                        .cost(unitCost)
                        .customerId(ctx.getCustomerId())
                        .customerGroupId(ctx.getCustomerGroupId())
                        .pricingTime(LocalDateTime.now())
                        .policy(ctx.getPricingPolicy())
                        .build()
        );

        BigDecimal unitPrice = pricing.getFinalPrice();
        BigDecimal gross = unitPrice.multiply(BigDecimal.valueOf(ctx.getQuantity()));

        // =========================
        // 5. TAX
        // =========================
        var taxState = taxSystemStateService.getOrCreate(ctx.getBranchId());

        boolean vatEnabled = taxState.isVatEnabled();
        BigDecimal vatRate = vatEnabled ? taxState.getVatRate() : BigDecimal.ZERO;
        boolean inclusive = taxState.isPricesVatInclusive();

        BigDecimal net;
        BigDecimal vat;

        if (vatEnabled) {

            if (inclusive) {
                net = gross.divide(BigDecimal.ONE.add(vatRate), 6, RoundingMode.HALF_UP);
                vat = gross.subtract(net);
            } else {
                net = gross;
                vat = net.multiply(vatRate);
                gross = net.add(vat);
            }

        } else {
            net = gross;
            vat = BigDecimal.ZERO;
        }

        // =========================
        // 6. SERIALIZATION
        // =========================
        String pricingJson;
        try {
            pricingJson = objectMapper.writeValueAsString(pricing);
        } catch (Exception e) {
            pricingJson = "{}";
        }

        return SellableSnapshot.builder()
                .productVariantId(ctx.getProductVariantId())
                .packagingId(packaging.getId())
                .quantity(ctx.getQuantity())
                .baseUnits(baseUnits)
                .unitPrice(unitPrice)
                .totalPrice(gross)
                .unitCost(unitCost)
                .totalCost(totalCost)
                .netAmount(net)
                .vatAmount(vat)
                .vatRate(vatRate)
                .availableStock(available)
                .allocations(
                        allocation != null
                                ? allocation.getAllocations()
                                : null
                )
                .adjustments(pricing.getAdjustments())
                .stockSufficient(sufficient)
                .warnings(warnings)
                .pricingJson(pricingJson)
                .resolutionMode(ctx.getMode().name())
                .build();
    }
}