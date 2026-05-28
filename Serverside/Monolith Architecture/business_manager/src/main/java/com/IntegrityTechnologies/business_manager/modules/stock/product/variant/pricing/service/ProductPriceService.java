package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.service;

import com.IntegrityTechnologies.business_manager.config.caffeine.CacheInvalidationService;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.service.InventoryService;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.model.ProductVariant;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.packaging.model.ProductVariantPackaging;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.model.ProductPrice;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.repository.ProductPriceRepository;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import jakarta.persistence.EntityManager;
import jakarta.persistence.EntityNotFoundException;
import jakarta.persistence.PersistenceContext;
import lombok.RequiredArgsConstructor;
import org.springframework.cache.annotation.Cacheable;
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
    private final InventoryService inventoryService;

    @PersistenceContext
    private EntityManager em;

    private UUID tenantId() {
        return TenantContext.getTenantId();
    }

    /* =====================================================
       RESOLVE PRICE
    ===================================================== */

    @Cacheable(
            value = "pricing-preview",
            key = "T(com.IntegrityTechnologies.business_manager.config.caffeine.CacheKeys)" +
                    ".pricing(" +
                    "T(com.IntegrityTechnologies.business_manager.security.util.TenantContext)" +
                    ".getTenantId()," +
                    "#variantId," +
                    "#packagingId," +
                    "#branchId," +
                    "null," +
                    "null," +
                    "#quantity)"
    )
    public ProductPrice resolvePrice(
            UUID branchId,
            UUID variantId,
            UUID packagingId,
            Long quantity
    ) {

        List<ProductPrice> prices =
                priceRepo.findApplicablePrices(
                        variantId,
                        packagingId,
                        tenantId(),
                        branchId,
                        quantity
                );

        if (prices.isEmpty()) {
            throw new EntityNotFoundException(
                    "No pricing configured for this variant/packaging/quantity"
            );
        }

        return prices.get(0);
    }

    @Transactional(readOnly = true)
    public List<ProductPrice> getPricesForVariant(
            UUID branchId,
            UUID variantId
    ) {

        return priceRepo
                .findByProductVariantIdAndTenantIdAndBranchIdAndDeletedFalse(
                        variantId,
                        tenantId(),
                        branchId
                );
    }

    @Transactional
    public ProductPrice createPrice(
            UUID branchId,
            UUID variantId,
            UUID packagingId,
            BigDecimal price,
            Long minQty
    ) {

        validatePriceAgainstCost(
                branchId,
                variantId,
                price
        );

        ProductPrice p = ProductPrice.builder()
                .productVariant(
                        em.getReference(ProductVariant.class, variantId)
                )
                .packaging(
                        em.getReference(ProductVariantPackaging.class, packagingId)
                )
                .price(price)
                .minQuantity(minQty)
                .tenantId(tenantId())
                .branchId(branchId)
                .build();

        cacheInvalidationService.evictPricingByVariant(
                tenantId(),
                variantId
        );

        return priceRepo.save(p);
    }

    @Transactional
    public ProductPrice updatePrice(
            UUID branchId,
            UUID id,
            BigDecimal price
    ) {

        ProductPrice p =
                priceRepo.findById(id)
                        .orElseThrow(() ->
                                new EntityNotFoundException("Price not found")
                        );

        if (!branchId.equals(p.getBranchId())) {
            throw new SecurityException(
                    "Cross-branch pricing modification denied"
            );
        }

        validatePriceAgainstCost(
                branchId,
                p.getProductVariant().getId(),
                price
        );

        p.setPrice(price);

        cacheInvalidationService.evictPricingByVariant(
                tenantId(),
                p.getProductVariant().getId()
        );

        return priceRepo.save(p);
    }

    public void validatePriceAgainstCost(
            UUID branchId,
            UUID variantId,
            BigDecimal price
    ) {

        BigDecimal avgCost =
                inventoryService.getAverageCost(
                        variantId,
                        branchId
                );

        if (
                avgCost != null
                        && avgCost.compareTo(BigDecimal.ZERO) > 0
                        && price.compareTo(avgCost) < 0
        ) {

            throw new IllegalStateException(
                    "Selling price cannot be below cost"
            );
        }
    }

    @Transactional
    public void deletePrice(
            UUID branchId,
            UUID id
    ) {

        ProductPrice p =
                priceRepo.findById(id)
                        .orElseThrow(() ->
                                new EntityNotFoundException("Price not found")
                        );

        if (!branchId.equals(p.getBranchId())) {
            throw new SecurityException(
                    "Cross-branch pricing deletion denied"
            );
        }

        p.setDeleted(true);
        p.setDeletedAt(LocalDateTime.now());

        cacheInvalidationService.evictPricingByVariant(
                tenantId(),
                p.getProductVariant().getId()
        );

        priceRepo.save(p);
    }
}