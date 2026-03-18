package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.service;

import com.IntegrityTechnologies.business_manager.exception.EntityNotFoundException;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.dto.BarcodeScanResponse;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.dto.VariantScanProjection;
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

        return BarcodeScanResponse.builder()
                .productId(p.getProductId())
                .productName(p.getProductName())
                .variantId(p.getVariantId())
                .classification(p.getClassification())
                .sku(p.getSku())
                .barcode(p.getBarcode())
                .sellingPrice(p.getSellingPrice())
                .branchId(p.getBranchId())
                .quantityOnHand(p.getQuantityOnHand())
                .build();
    }
}