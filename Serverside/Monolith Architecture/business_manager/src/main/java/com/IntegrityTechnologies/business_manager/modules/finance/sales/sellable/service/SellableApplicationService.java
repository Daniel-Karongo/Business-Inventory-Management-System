package com.IntegrityTechnologies.business_manager.modules.finance.sales.sellable.service;

import com.IntegrityTechnologies.business_manager.modules.finance.sales.sellable.dto.SellableResolveRequest;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.sellable.dto.SellableResolveResponse;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.packaging.model.ProductVariantPackaging;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.packaging.service.ProductVariantPackagingService;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class SellableApplicationService {

    private final SellableResolverService resolver;
    private final ProductVariantPackagingService packagingService;

    public SellableResolveResponse resolve(SellableResolveRequest req) {

        ProductVariantPackaging packaging =
                (req.getPackagingId() != null)
                        ? packagingService.getPackagings(req.getProductVariantId())
                        .stream()
                        .filter(p -> p.getId().equals(req.getPackagingId()))
                        .findFirst()
                        .orElseThrow(() -> new IllegalArgumentException("Invalid packaging"))
                        : packagingService.getBasePackaging(req.getProductVariantId());

        long quantity = req.getQuantity() != null ? req.getQuantity() : 1L;

        return resolver.resolveFull(
                req.getProductVariantId(),
                packaging.getId(),
                quantity,
                req.getBranchId(),
                req.getCustomerId(),
                req.getCustomerGroupId(),
                packaging.getUnitsPerPackaging(),
                req.getBatchIds()
        );
    }
}