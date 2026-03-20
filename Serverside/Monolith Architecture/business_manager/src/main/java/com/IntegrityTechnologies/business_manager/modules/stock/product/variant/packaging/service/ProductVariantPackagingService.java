package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.packaging.service;

import com.IntegrityTechnologies.business_manager.exception.EntityNotFoundException;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.sellable.cache.CacheInvalidationService;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.sellable.dto.PackagingDTO;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.model.ProductVariant;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.packaging.model.ProductVariantPackaging;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.packaging.model.ProductVariantPackagingAudit;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.packaging.repository.ProductVariantPackagingAuditRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.packaging.repository.ProductVariantPackagingRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.repository.ProductVariantRepository;
import com.IntegrityTechnologies.business_manager.security.util.BranchContext;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.*;

@Service
@RequiredArgsConstructor
public class ProductVariantPackagingService {

    private final ProductVariantPackagingRepository packagingRepo;
    private final ProductVariantRepository variantRepo;
    private final ProductVariantPackagingAuditRepository auditRepo;
    private final CacheInvalidationService cacheInvalidationService;

    private UUID tenantId() { return TenantContext.getTenantId(); }
    private UUID branchId() { return BranchContext.get(); }

    /* =====================================================
       CREATE PACKAGING
    ===================================================== */

    @Transactional
    public ProductVariantPackaging createPackaging(
            UUID variantId,
            String name,
            Long unitsPerPackaging
    ) {

        ProductVariant variant = variantRepo.findByIdSafe(
                variantId,
                false,
                tenantId(),
                branchId()
        ).orElseThrow(() -> new EntityNotFoundException("Variant not found"));

        if (unitsPerPackaging == null || unitsPerPackaging <= 0) {
            throw new IllegalArgumentException("unitsPerPackaging must be > 0");
        }

        if (unitsPerPackaging == 1) {
            throw new IllegalArgumentException(
                    "unitsPerPackaging = 1 is reserved for base unit"
            );
        }

        ProductVariantPackaging packaging = ProductVariantPackaging.builder()
                .productVariant(variant)
                .name(name)
                .unitsPerPackaging(unitsPerPackaging)
                .isBaseUnit(false)
                .tenantId(tenantId())
                .branchId(branchId())
                .build();

        cacheInvalidationService.evictPackaging(variantId);
        cacheInvalidationService.evictPricingByVariant(variantId, branchId());

        return packagingRepo.save(packaging);
    }

    /* =====================================================
       GET PACKAGINGS
    ===================================================== */
    public Map<UUID, List<PackagingDTO>> getPackagingsBulk(List<UUID> variantIds) {

        Map<UUID, List<PackagingDTO>> map = new HashMap<>();

        for (UUID variantId : variantIds) {

            List<ProductVariantPackaging> packagings = getPackagings(variantId); // cached

            List<PackagingDTO> dtos = packagings.stream()
                    .map(p -> PackagingDTO.builder()
                            .packagingId(p.getId())
                            .name(p.getName())
                            .unitsPerPackaging(p.getUnitsPerPackaging())
                            .isBaseUnit(p.getIsBaseUnit())
                            .build()
                    )
                    .toList();

            map.put(variantId, dtos);
        }

        return map;
    }

    @Cacheable(
            value = "packaging",
            key = "T(com.IntegrityTechnologies.business_manager.modules.finance.sales.sellable.cache.SellableCacheKey).packaging(#variantId)"
    )
    public List<ProductVariantPackaging> getPackagings(UUID variantId) {
        return packagingRepo.findByProductVariantIdAndDeletedFalse(variantId);
    }

    public ProductVariantPackaging getBasePackaging(UUID variantId) {
        ProductVariantPackaging base =
                packagingRepo.findByProductVariantIdAndIsBaseUnitTrueAndDeletedFalse(variantId);

        if (base == null) {
            throw new IllegalStateException("Base packaging missing (data corruption)");
        }

        return base;
    }

    /* =====================================================
       UPDATE PACKAGING
    ===================================================== */

    @Transactional
    public ProductVariantPackaging updatePackaging(
            UUID packagingId,
            String name,
            Long unitsPerPackaging
    ) {

        ProductVariantPackaging packaging = packagingRepo.findById(packagingId)
                .orElseThrow(() -> new EntityNotFoundException("Packaging not found"));

        String oldPackaging = packaging.getProductVariant().getClassification() + " | " + packaging.getName() + " | " + packaging.getUnitsPerPackaging() + " units per packaging" + " | " + "base unit: " + packaging.getIsBaseUnit().toString();

        if (packaging.getDeleted()) {
            throw new IllegalStateException("Cannot update deleted packaging");
        }

        // ✅ Rename allowed (including base unit)
        if (name != null) {
            packaging.setName(name);
        }

        // ❗ Units rules
        if (unitsPerPackaging != null) {

            if (packaging.getIsBaseUnit()) {
                if (unitsPerPackaging != 1) {
                    throw new IllegalStateException(
                            "Base unit must always have unitsPerPackaging = 1"
                    );
                }
            } else {
                if (unitsPerPackaging <= 1) {
                    throw new IllegalArgumentException(
                            "Non-base packaging must have unitsPerPackaging > 1"
                    );
                }
                packaging.setUnitsPerPackaging(unitsPerPackaging);
            }
        }

        String newPackaging = packaging.getProductVariant().getClassification() + " | " + packaging.getName() + " | " + packaging.getUnitsPerPackaging() + " units per packaging" + " | " + "base unit: " + packaging.getIsBaseUnit().toString();

        auditRepo.save(
                ProductVariantPackagingAudit.builder()
                        .tenantId(tenantId())
                        .branchId(branchId())
                        .productVariantId(packaging.getProductVariant().getId())
                        .packagingName(packaging.getName())
                        .action("UPDATE")
                        .fieldChanged("units/name")
                        .oldValue(oldPackaging)
                        .newValue(newPackaging)
                        .timestamp(LocalDateTime.now())
                        .performedBy("SYSTEM")
                        .build()
        );

        UUID variantId = packaging.getProductVariant().getId();

        cacheInvalidationService.evictPackaging(variantId);
        cacheInvalidationService.evictPricingByVariant(variantId, branchId());
        
        return packagingRepo.save(packaging);
    }

    /* =====================================================
       DELETE PACKAGING
    ===================================================== */

    @Transactional
    @CacheEvict(value = "packaging", allEntries = true)
    public void deletePackaging(UUID packagingId) {

        ProductVariantPackaging packaging = packagingRepo.findById(packagingId)
                .orElseThrow(() -> new EntityNotFoundException("Packaging not found"));

        if (packaging.getIsBaseUnit()) {
            throw new IllegalStateException("Cannot delete base packaging");
        }

        packaging.setDeleted(true);
        packaging.setDeletedAt(java.time.LocalDateTime.now());

        cacheInvalidationService.evictPackaging(packaging.getProductVariant().getId());
        cacheInvalidationService.evictPricingByVariant(packaging.getProductVariant().getId(), branchId());

        packagingRepo.save(packaging);
    }

    public long toBaseUnits(UUID packagingId, long quantity) {
        ProductVariantPackaging packaging = packagingRepo.findById(packagingId)
                .orElseThrow(() -> new EntityNotFoundException("Packaging not found"));

        if (packaging.getDeleted()) {
            throw new IllegalStateException("Packaging is deleted");
        }

        return quantity * packaging.getUnitsPerPackaging();
    }
}