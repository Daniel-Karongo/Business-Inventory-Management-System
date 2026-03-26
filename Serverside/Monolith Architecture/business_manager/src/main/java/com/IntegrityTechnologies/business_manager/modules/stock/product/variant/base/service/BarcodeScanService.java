package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.service;

import com.IntegrityTechnologies.business_manager.exception.EntityNotFoundException;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.sellable.domain.ResolutionMode;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.sellable.domain.SellableContext;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.sellable.domain.SellableSnapshot;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.sellable.service.SellableResolutionService;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.dto.BarcodeScanResponse;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.dto.VariantScanProjection;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.packaging.model.ProductVariantPackaging;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.packaging.service.ProductVariantPackagingService;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.repository.ProductVariantRepository;
import com.IntegrityTechnologies.business_manager.security.util.BranchContext;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.stereotype.Service;

import java.util.UUID;

@Service
@RequiredArgsConstructor
public class BarcodeScanService {

    private final ProductVariantRepository variantRepo;
    private final SellableResolutionService resolutionService;
    private final ProductVariantPackagingService packagingService;

    private UUID tenantId() { return TenantContext.getTenantId(); }
    private UUID branchId() { return BranchContext.get(); }

    @Cacheable(
            value = "barcode-scan",
            key = "T(java.util.Objects).hash(#root.target.tenantId(), #root.target.branchId(), #barcode)"
    )
    public BarcodeScanResponse scan(String barcode) {

        VariantScanProjection p = variantRepo
                .scanProjection(tenantId(), branchId(), barcode)
                .orElseThrow(() ->
                        new EntityNotFoundException("Invalid barcode or SKU")
                );

        // =========================
        // BASE PACKAGING
        // =========================
        ProductVariantPackaging packaging =
                packagingService.getBasePackaging(p.getVariantId());

        long quantity = 1L;

        // =========================
        // RESOLVE (NEW ENGINE)
        // =========================
        SellableSnapshot snap = resolutionService.resolve(
                SellableContext.builder()
                        .tenantId(tenantId())
                        .branchId(branchId())
                        .productVariantId(p.getVariantId())
                        .packagingId(packaging.getId())
                        .quantity(quantity)
                        .mode(ResolutionMode.UI_FAST) // 🔥 correct for scan
                        .build()
        );

        return BarcodeScanResponse.builder()
                .productId(p.getProductId())
                .productName(p.getProductName())
                .variantId(p.getVariantId())
                .classification(p.getClassification())
                .sku(p.getSku())
                .barcode(p.getBarcode())
                .branchId(p.getBranchId())

                .packagingId(packaging.getId())
                .requestedQuantity(quantity)
                .baseUnits(snap.getBaseUnits())
                .unitPrice(snap.getUnitPrice())
                .totalPrice(snap.getTotalPrice())
                .availableStock(snap.getAvailableStock())
                .totalCost(snap.getTotalCost())
                .batchAllocations(snap.getAllocations())
                .adjustments(snap.getAdjustments())

                .build();
    }
}