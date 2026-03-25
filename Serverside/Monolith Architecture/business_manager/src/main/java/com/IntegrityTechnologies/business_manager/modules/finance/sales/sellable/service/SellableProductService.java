package com.IntegrityTechnologies.business_manager.modules.finance.sales.sellable.service;

import com.IntegrityTechnologies.business_manager.config.response.PageWrapper;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.sellable.dto.*;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.model.InventoryBatch;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.model.InventoryItem;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository.BatchReservationRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository.InventoryBatchRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository.InventoryItemRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.service.InventoryService;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.model.ProductVariant;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.repository.ProductVariantRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.packaging.service.ProductVariantPackagingService;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.util.*;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class SellableProductService {

    private final ProductVariantRepository variantRepository;
    private final InventoryItemRepository inventoryItemRepository;
    private final BatchReservationRepository batchReservationRepository;
    private final InventoryBatchRepository batchRepository;
    private final SellableResolverService resolver;
    private final InventoryService inventoryService;
    private final ProductVariantPackagingService packagingService;

    // =========================
    // ENTRY POINT
    // =========================
    public SellableProductResponse search(SellableProductRequest request) {

        UUID tenantId = TenantContext.getTenantId();
        UUID branchId = request.getBranchId();
        long quantity = request.getQuantity() != null ? request.getQuantity() : 1L;

        int page = request.getPage() != null ? request.getPage() : 0;
        int size = request.getSize() != null ? request.getSize() : 20;

        Pageable pageable = PageRequest.of(page, size);

        // =========================================
        // 🔥 PAGINATED VARIANT FETCH
        // =========================================
        Page<ProductVariant> variantPage =
                fetchVariantsPaged(tenantId, branchId, request.getSearch(), pageable);

        List<ProductVariant> variants = variantPage.getContent();

        if (variants.isEmpty()) {
            return SellableProductResponse.builder()
                    .variants(new PageWrapper<>(variantPage.map(v -> null)))
                    .build();
        }

        List<UUID> variantIds =
                variants.stream().map(ProductVariant::getId).toList();

        // =========================================
        // BULK LOAD
        // =========================================

        Map<UUID, InventoryItem> inventoryMap =
                inventoryItemRepository.findAllByVariants(variantIds, tenantId, branchId)
                        .stream()
                        .collect(Collectors.toMap(
                                InventoryItem::getProductVariantId,
                                i -> i
                        ));

        Map<String, Long> reservedMap = new HashMap<>();

        batchReservationRepository
                .sumReservedBulk(variantIds, tenantId)
                .forEach(row -> {

                    String key = row.getProductVariantId() + "|" + row.getBranchId();

                    reservedMap.put(key, row.getTotalReserved());
                });

        Map<UUID, List<PackagingDTO>> packagingMap =
                packagingService.getPackagingsBulk(variantIds);

        // =========================================
        // BUILD DTOs
        // =========================================

        List<SellableVariantDTO> dtoList = variants.stream()
                .map(v -> buildVariantDTOOptimized(
                        v,
                        request,
                        quantity,
                        inventoryMap,
                        reservedMap,
                        packagingMap
                ))
                .toList();

        Page<SellableVariantDTO> dtoPage =
                new PageImpl<>(dtoList, pageable, variantPage.getTotalElements());

        return SellableProductResponse.builder()
                .variants(new PageWrapper<>(dtoPage))
                .build();
    }

    @Cacheable(
            value = "variant-search",
            key = "T(com.IntegrityTechnologies.business_manager.modules.finance.sales.sellable.cache.SellableCacheKey)" +
                    ".variants(#branchId, #search, #pageable.pageNumber, #pageable.pageSize)"
    )
    public Page<ProductVariant> fetchVariantsPaged(
            UUID tenantId,
            UUID branchId,
            String search,
            Pageable pageable
    ) {
        return variantRepository.searchSellablePaged(
                tenantId,
                branchId,
                search == null || search.isBlank() ? null : search,
                pageable
        );
    }

    // =========================
    // BUILD VARIANT DTO
    // =========================
    private SellableVariantDTO buildVariantDTOOptimized(
            ProductVariant variant,
            SellableProductRequest request,
            long quantity,
            Map<UUID, InventoryItem> inventoryMap,
            Map<String, Long> reservedMap,
            Map<UUID, List<PackagingDTO>> packagingMap
    ) {

        InventoryItem item = inventoryMap.get(variant.getId());

        long onHand = item != null ? item.getQuantityOnHand() : 0L;
        long reserved = reservedMap.getOrDefault(
                variant.getId() + "|" + request.getBranchId(),
                0L
        );
        long available = Math.max(0, onHand - reserved);

        List<PackagingDTO> packagingList =
                packagingMap.getOrDefault(variant.getId(), List.of());

        Map<UUID, PricingPreviewDTO> pricingMap = new HashMap<>();

        if (request.isIncludePricing()) {
            for (PackagingDTO pkg : packagingList) {

                ResolvedSellable resolved = resolver.resolve(
                        variant.getId(),
                        pkg.getPackagingId(),
                        quantity,
                        request.getBranchId(),
                        request.getCustomerId(),
                        request.getCustomerGroupId(),
                        pkg.getUnitsPerPackaging()
                );

                pricingMap.put(
                        pkg.getPackagingId(),
                        PricingPreviewDTO.builder()
                                .unitPrice(resolved.getUnitPrice())
                                .totalPrice(resolved.getTotalPrice())
                                .adjustments(resolved.getAdjustments())
                                .build()
                );
            }
        }

        // Allocation (still per variant → acceptable)
        AllocationPreviewDTO allocation = null;
        if (request.isIncludeAllocation() && available > 0) {

            long baseUnits = quantity; // fallback

            if (!packagingList.isEmpty()) {
                baseUnits = quantity * packagingList.get(0).getUnitsPerPackaging();
            }

            Map<String, Object> alloc =
                    inventoryService.previewAllocation(
                            variant.getId(),
                            request.getBranchId(),
                            baseUnits,
                            null
                    );

            allocation = AllocationPreviewDTO.builder()
                    .totalCost((BigDecimal) alloc.get("totalCost"))
                    .allocations((List<Map<String, Object>>) alloc.get("allocations"))
                    .build();
        }

        // Batches (optional)
        List<BatchPreviewDTO> batches = null;
        if (request.isIncludeBatches()) {
            batches = buildBatchPreview(variant.getId(), request.getBranchId());
        }

        List<WarningDTO> warnings =
                buildWarnings(available, pricingMap, variant);

        return SellableVariantDTO.builder()
                .productId(variant.getProduct().getId())
                .productName(variant.getProduct().getName())
                .productSku(variant.getProduct().getSku())
                .variantId(variant.getId())
                .variantSku(variant.getSku())
                .classification(variant.getClassification())
                .quantityOnHand(onHand)
                .quantityReserved(reserved)
                .quantityAvailable(available)
                .packagings(packagingList)
                .pricingByPackaging(pricingMap)
                .batches(batches)
                .allocation(allocation)
                .warnings(warnings)
                .build();
    }

    // =========================
    // PRICING
    // =========================
    private List<BatchPreviewDTO> buildBatchPreview(UUID variantId, UUID branchId) {

        Map<UUID, Long> reservedMap =
                inventoryService.computeReservedPerBatch(variantId, branchId);

        List<InventoryBatch> batches =
                batchRepository.findAvailableBatches(
                        variantId,
                        TenantContext.getTenantId(),
                        branchId
                );

        return batches.stream()
                .limit(5)
                .map(b -> {

                    long reserved = reservedMap.getOrDefault(b.getId(), 0L);
                    long available = b.getQuantityRemaining() - reserved;

                    return BatchPreviewDTO.builder()
                            .batchId(b.getId())
                            .available(available)
                            .unitCost(b.getUnitCost())
                            .receivedAt(b.getReceivedAt())
                            .build();
                })
                .toList();
    }

    // =========================
    // WARNINGS ENGINE
    // =========================
    private List<WarningDTO> buildWarnings(
            long available,
            Map<UUID, PricingPreviewDTO> pricingMap,
            ProductVariant variant
    ) {

        List<WarningDTO> warnings = new ArrayList<>();

        if (available <= 0) {
            warnings.add(WarningDTO.builder()
                    .type("OUT_OF_STOCK")
                    .message("Item is out of stock")
                    .build());
        } else if (available <= 5) {
            warnings.add(WarningDTO.builder()
                    .type("LOW_STOCK")
                    .message("Low stock remaining")
                    .build());
        }

        return warnings;
    }
}