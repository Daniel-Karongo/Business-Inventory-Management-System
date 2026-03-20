package com.IntegrityTechnologies.business_manager.modules.finance.sales.service;

import com.IntegrityTechnologies.business_manager.modules.finance.sales.dto.BatchSelectionDto;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.dto.SaleLinePreviewRequest;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.dto.SaleLinePreviewResponse;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.service.InventoryService;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.packaging.model.ProductVariantPackaging;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.packaging.service.ProductVariantPackagingService;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.model.PricingContext;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.model.PricingResult;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.service.PricingEngineService;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class SalePreviewService {

    private final PricingEngineService pricingEngine;
    private final ProductVariantPackagingService packagingService;
    private final InventoryService inventoryService;

    public SaleLinePreviewResponse preview(SaleLinePreviewRequest req) {

        // =====================================================
        // 1. PACKAGING
        // =====================================================
        ProductVariantPackaging packaging =
                (req.getPackagingId() != null)
                        ? packagingService.getPackagings(req.getProductVariantId())
                        .stream()
                        .filter(p -> p.getId().equals(req.getPackagingId()))
                        .findFirst()
                        .orElseThrow(() -> new IllegalArgumentException("Invalid packaging"))
                        : packagingService.getBasePackaging(req.getProductVariantId());

        long baseUnits = req.getQuantity() * packaging.getUnitsPerPackaging();

        // =====================================================
        // 2. PRICING
        // =====================================================
        PricingResult pricing = pricingEngine.resolve(
                PricingContext.builder()
                        .productVariantId(req.getProductVariantId())
                        .packagingId(packaging.getId())
                        .quantity(req.getQuantity())
                        .customerId(req.getCustomerId())
                        .customerGroupId(req.getCustomerGroupId())
                        .branchId(req.getBranchId())
                        .pricingTime(LocalDateTime.now())
                        .build()
        );

        // =====================================================
        // 3. STOCK
        // =====================================================
        long available = inventoryService.availableQuantity(
                req.getProductVariantId(),
                req.getBranchId()
        );

        if (available < baseUnits) {
            throw new IllegalStateException("Insufficient stock");
        }

        // =====================================================
        // 4. BATCH PREVIEW
        // =====================================================
        List<UUID> batchIds = req.getBatchSelections() == null
                ? null
                : req.getBatchSelections()
                .stream()
                .map(BatchSelectionDto::getBatchId)
                .toList();

        Map<String, Object> allocation =
                inventoryService.previewAllocation(
                        req.getProductVariantId(),
                        req.getBranchId(),
                        baseUnits,
                        batchIds
                );

        BigDecimal totalCost = (BigDecimal) allocation.get("totalCost");

        return SaleLinePreviewResponse.builder()
                .productVariantId(req.getProductVariantId())
                .packagingId(packaging.getId())
                .requestedQuantity(req.getQuantity())
                .baseUnits(baseUnits)
                .unitPrice(pricing.getFinalPrice())
                .totalPrice(
                        pricing.getFinalPrice()
                                .multiply(BigDecimal.valueOf(req.getQuantity()))
                )
                .totalCost(totalCost)
                .availableStock(available)
                .batchAllocations((java.util.List<Map<String, Object>>) allocation.get("allocations"))
                .adjustments(pricing.getAdjustments())
                .build();
    }
}