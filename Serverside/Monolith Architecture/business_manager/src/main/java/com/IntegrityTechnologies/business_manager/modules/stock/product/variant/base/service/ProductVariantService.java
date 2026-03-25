package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.service;

import com.IntegrityTechnologies.business_manager.exception.EntityNotFoundException;
import com.IntegrityTechnologies.business_manager.config.caffeine.CacheInvalidationService;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository.BatchConsumptionRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository.BatchReservationRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository.InventoryItemRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.dto.ProductVariantCreateDTO;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.dto.ProductVariantDTO;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.dto.ProductVariantUpdateDTO;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.mapper.ProductVariantMapper;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.model.Product;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.model.ProductVariant;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.repository.ProductRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.packaging.model.ProductVariantPackaging;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.packaging.repository.ProductVariantPackagingRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.repository.ProductVariantRepository;
import com.IntegrityTechnologies.business_manager.security.util.BranchContext;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class ProductVariantService {
    /**
     * IMPORTANT DOMAIN NOTE:
     *
     * ProductVariant is NOT directly sellable.
     *
     * Sellable = Variant + Packaging + Pricing + Adjustments
     *
     * Do NOT use ProductVariant as a source of pricing or stock decisions.
     * Always use SellableResolverService.
     */

    private final ProductVariantRepository variantRepo;
    private final ProductRepository productRepo;
    private final ProductVariantMapper mapper;
    private final VariantBarcodeService barcodeService;
    private final InventoryItemRepository inventoryItemRepository;
    private final ProductVariantPackagingRepository packagingRepo;
    private final CacheInvalidationService cacheInvalidationService;
    private final BatchReservationRepository batchReservationRepository;
    private final BatchConsumptionRepository batchConsumptionRepository;

    private UUID tenantId() { return TenantContext.getTenantId(); }
    private UUID branchId() { return BranchContext.get(); }

    @Transactional
    public ProductVariantDTO createVariant(ProductVariantCreateDTO dto) {

        Product product = productRepo.findByIdAndTenantIdAndBranchIdAndDeletedFalse(
                dto.getProductId(),
                tenantId(),
                branchId()
        ).orElseThrow(() -> new EntityNotFoundException("Product not found"));

        if (variantRepo.existsByTenantIdAndBranchIdAndProduct_IdAndClassification(
                tenantId(),
                branchId(),
                dto.getProductId(),
                dto.getClassification()
        )) {
            throw new IllegalArgumentException("Variant already exists");
        }

        String sku =
                dto.getSku() != null
                        ? dto.getSku()
                        : product.getSku() + "-" + normalize(dto.getClassification());

        if (variantRepo.existsByTenantIdAndBranchIdAndSku(
                tenantId(),
                branchId(),
                sku
        )) {
            throw new IllegalArgumentException("SKU already exists: " + sku);
        }

        ProductVariant v = new ProductVariant();
        v.setProduct(product);
        v.setClassification(dto.getClassification());
        v.setSku(sku);
        v.setTenantId(tenantId());
        v.setBranchId(branchId());
        v.setMinimumPercentageProfit(dto.getMinimumPercentageProfit());
        v.setMinimumProfit(dto.getMinimumProfit());

        v = variantRepo.save(v);

        // ✅ CRITICAL: ensure base packaging exists
        createBasePackaging(v);

        cacheInvalidationService.evictVariantSearch(branchId());
        cacheInvalidationService.evictPricingByVariant(v.getId(), branchId());

        return barcodeService.generateBarcodeIfMissing(v.getId());
    }

    private void createBasePackaging(ProductVariant variant) {

        var existing = packagingRepo
                .findByProductVariantIdAndIsBaseUnitTrueAndDeletedFalse(variant.getId());

        if (existing != null) return;

        ProductVariantPackaging packaging = ProductVariantPackaging.builder()
                .productVariant(variant)
                .name("Piece")
                .unitsPerPackaging(1L)
                .isBaseUnit(true)
                .tenantId(tenantId())
                .branchId(branchId())
                .build();

        packagingRepo.save(packaging);
    }

    public ProductVariant getEntity(UUID id) {
        return variantRepo.findByIdSafe(
                id,
                false,
                tenantId(),
                branchId()
        ).orElseThrow(() -> new EntityNotFoundException("Variant not found"));
    }

    public List<ProductVariant> getEntitiesForProduct(UUID productId) {
        return variantRepo.findByProduct_IdSafe(
                productId,
                false,
                tenantId(),
                branchId()
        );
    }

    public ProductVariantDTO getVariant(UUID id) {
        return mapper.toDTO(getEntity(id));
    }

    public List<ProductVariantDTO> getVariantsForProduct(UUID productId) {
        return getEntitiesForProduct(productId)
                .stream()
                .map(mapper::toDTO)
                .toList();
    }

    // NOTE:
    // minimumSellingPrice is ONLY a fallback / warning reference.
    // PricingEngine is the source of truth for actual selling price.
    @Transactional
    @CacheEvict(value = "pricing-preview", allEntries = true)
    public ProductVariantDTO updateVariant(UUID id, ProductVariantUpdateDTO dto) {

        ProductVariant v = getEntity(id);

        if (dto.getClassification() != null &&
                !dto.getClassification().equals(v.getClassification())) {

            if (variantRepo.existsByTenantIdAndBranchIdAndProduct_IdAndClassification(
                    tenantId(),
                    branchId(),
                    v.getProduct().getId(),
                    dto.getClassification()
            )) {
                throw new IllegalArgumentException("Variant classification already exists");
            }

            v.setClassification(dto.getClassification());
        }

        if (dto.getSku() != null && !dto.getSku().equals(v.getSku())) {

            if (variantRepo.existsByTenantIdAndBranchIdAndSku(
                    tenantId(),
                    branchId(),
                    dto.getSku()
            )) {
                throw new IllegalArgumentException("SKU already exists: " + dto.getSku());
            }

            v.setSku(dto.getSku());
        }

        v.setMinimumPercentageProfit(dto.getMinimumPercentageProfit());
        v.setMinimumProfit(dto.getMinimumProfit());

        cacheInvalidationService.evictVariantSearch(branchId());
        cacheInvalidationService.evictPricingByVariant(v.getId(), branchId());

        return mapper.toDTO(variantRepo.save(v));
    }

    @Transactional
    public void deleteVariant(UUID variantId) {

        ProductVariant variant = getEntity(variantId);

        UUID productId = variant.getProduct().getId();

        boolean hasNonBasePackaging =
                packagingRepo.existsByProductVariantIdAndIsBaseUnitFalseAndDeletedFalse(variantId);

        if (hasNonBasePackaging) {
            throw new IllegalStateException("Cannot delete variant with additional packaging configurations");
        }

        boolean hasReservations =
                batchReservationRepository.existsByProductVariantIdAndTenantIdAndBranchId(
                        variantId,
                        tenantId(),
                        branchId()
                );

        boolean hasConsumptions =
                batchConsumptionRepository.existsByProductVariantIdAndTenantIdAndBranchId(
                        variantId,
                        tenantId(),
                        branchId()
                );

        if (hasReservations || hasConsumptions) {
            throw new IllegalStateException("Cannot delete variant with stock history");
        }
        
        boolean hasInventory =
                inventoryItemRepository.existsByProductVariantIdAndTenantIdAndBranchIdAndDeletedFalse(
                        variantId,
                        tenantId(),
                        branchId()
                );

        if (hasInventory) {
            throw new IllegalStateException(
                    "Cannot delete variant '" +
                            variant.getClassification() +
                            "' because inventory exists."
            );
        }

        long activeVariants =
                variantRepo.countByTenantIdAndBranchIdAndProduct_IdAndDeletedFalse(
                        tenantId(),
                        branchId(),
                        productId
                );

        if (activeVariants <= 1) {
            throw new IllegalStateException(
                    "Cannot delete the last remaining variant."
            );
        }

        variant.setDeleted(true);
        variantRepo.save(variant);

        cacheInvalidationService.evictVariantSearch(branchId());
        cacheInvalidationService.evictPricingByVariant(variant.getId(), branchId());
        cacheInvalidationService.evictPackaging(variantId);
    }

    // SKU is immutable once created unless explicitly overridden
    private String normalize(String input) {
        return input == null ? "" :
                input.replaceAll("[^A-Za-z0-9]", "")
                        .toUpperCase();
    }

    public boolean isSellable(UUID variantId) {
        return packagingRepo.existsByProductVariantIdAndDeletedFalse(variantId);
    }
}