package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.service;

import com.IntegrityTechnologies.business_manager.exception.EntityNotFoundException;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.sellable.service.SellableResolverService;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.dto.BarcodeScanResponse;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.dto.VariantScanProjection;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.packaging.model.ProductVariantPackaging;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.packaging.service.ProductVariantPackagingService;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.repository.ProductVariantRepository;
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
    private final SellableResolverService resolver;
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
        // 1. DEFAULT PACKAGING (BASE)
        // =========================
        ProductVariantPackaging packaging =
                packagingService.getBasePackaging(p.getVariantId());

        long quantity = 1L; // default scan = 1 unit

        // =========================
        // 2. RESOLVE SELLABLE
        // =========================
        var resolved = resolver.resolveFull(
                p.getVariantId(),
                packaging.getId(),
                quantity,
                branchId(),
                null, // no customer in scan
                null,
                packaging.getUnitsPerPackaging(),
                null
        );

        return BarcodeScanResponse.builder()
                .productId(p.getProductId())
                .productName(p.getProductName())
                .variantId(p.getVariantId())
                .classification(p.getClassification())
                .sku(p.getSku())
                .barcode(p.getBarcode())
                .branchId(p.getBranchId())

                // ✅ NEW CORRECT DATA
                .packagingId(packaging.getId())
                .requestedQuantity(quantity)
                .baseUnits(resolved.getBaseUnits())
                .unitPrice(resolved.getUnitPrice())
                .totalPrice(resolved.getTotalPrice())
                .availableStock(resolved.getAvailableStock())
                .totalCost(resolved.getTotalCost())
                .batchAllocations(resolved.getBatchAllocations())
                .adjustments(resolved.getAdjustments())

                .build();
    }
}