package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.service;

import com.IntegrityTechnologies.business_manager.exception.EntityNotFoundException;
import com.IntegrityTechnologies.business_manager.config.caffeine.CacheInvalidationService;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.model.ProductVariant;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.packaging.model.ProductVariantPackaging;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.model.ProductPrice;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.repository.ProductPriceRepository;
import com.IntegrityTechnologies.business_manager.security.util.BranchContext;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class ProductPriceService {

    private final ProductPriceRepository priceRepo;
    private final CacheInvalidationService cacheInvalidationService;

    private UUID tenantId() { return TenantContext.getTenantId(); }
    private UUID branchId() { return BranchContext.get(); }

    /* =====================================================
       RESOLVE PRICE (CRITICAL METHOD)
    ===================================================== */

    public ProductPrice resolvePrice(
            UUID variantId,
            UUID packagingId,
            Long quantity
    ) {

        List<ProductPrice> prices = priceRepo.findApplicablePrices(
                variantId,
                packagingId,
                quantity
        );

        if (prices.isEmpty()) {
            throw new EntityNotFoundException(
                    "No pricing configured for this variant/packaging/quantity"
            );
        }

        return prices.get(0); // highest minQuantity match
    }

    @Transactional
    public ProductPrice createPrice(
            UUID variantId,
            UUID packagingId,
            BigDecimal price,
            Long minQty
    ) {
        ProductPrice p = ProductPrice.builder()
                .productVariant(ProductVariant.builder().id(variantId).build())
                .packaging(ProductVariantPackaging.builder().id(packagingId).build())
                .price(price)
                .minQuantity(minQty)
                .tenantId(tenantId())
                .branchId(branchId())
                .build();

        cacheInvalidationService.evictPricingByVariant(variantId, branchId());

        return priceRepo.save(p);
    }

    @Transactional
    public ProductPrice updatePrice(UUID id, BigDecimal price) {

        ProductPrice p = priceRepo.findById(id)
                .orElseThrow(() -> new EntityNotFoundException("Price not found"));

        p.setPrice(price);

        cacheInvalidationService.evictPricingByVariant(p.getProductVariant().getId(), branchId());

        return priceRepo.save(p);
    }

    @Transactional
    public void deletePrice(UUID id) {
        ProductPrice p = priceRepo.findById(id)
                .orElseThrow(() -> new EntityNotFoundException("Price not found"));

        p.setDeleted(true);
        p.setDeletedAt(LocalDateTime.now());

        cacheInvalidationService.evictPricingByVariant(p.getProductVariant().getId(), branchId());

        priceRepo.save(p);
    }
}