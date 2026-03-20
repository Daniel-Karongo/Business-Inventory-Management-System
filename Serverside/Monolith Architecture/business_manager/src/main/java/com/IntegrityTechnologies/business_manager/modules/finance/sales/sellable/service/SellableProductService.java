package com.IntegrityTechnologies.business_manager.modules.finance.sales.sellable.service;

import com.IntegrityTechnologies.business_manager.config.response.PageWrapper;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.sellable.dto.*;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.model.InventoryBatch;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.model.InventoryItem;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository.BatchReservationRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository.InventoryBatchRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository.InventoryItemRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.service.InventoryService;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.model.ProductVariant;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.packaging.service.ProductVariantPackagingService;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.model.PricingContext;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.model.PricingPolicy;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.model.PricingResult;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.service.PricingEngineService;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.repository.ProductVariantRepository;
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

    private final InventoryService inventoryService;
    private final PricingEngineService pricingEngineService;
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

        Map<UUID, Long> reservedMap = new HashMap<>();

        batchReservationRepository
                .sumReservedBulk(variantIds, tenantId, branchId)
                .forEach(row -> {
                    reservedMap.put((UUID) row[0], (Long) row[1]);
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
            Map<UUID, Long> reservedMap,
            Map<UUID, List<PackagingDTO>> packagingMap
    ) {

        InventoryItem item = inventoryMap.get(variant.getId());

        long onHand = item != null ? item.getQuantityOnHand() : 0L;
        long reserved = reservedMap.getOrDefault(variant.getId(), 0L);
        long available = Math.max(0, onHand - reserved);

        // Packaging (already mapped)
        List<PackagingDTO> packaging =
                packagingMap.getOrDefault(variant.getId(), List.of());

        // Pricing
        PricingPreviewDTO pricing = request.isIncludePricing()
                ? buildPricing(variant, request, quantity)
                : null;

        // Allocation (still per variant → acceptable)
        AllocationPreviewDTO allocation = null;
        if (request.isIncludeAllocation() && available > 0) {

            Map<String, Object> alloc =
                    inventoryService.previewAllocation(
                            variant.getId(),
                            request.getBranchId(),
                            quantity,
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
                buildWarnings(available, pricing, variant);

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
                .packagings(packaging)
                .pricing(pricing)
                .batches(batches)
                .allocation(allocation)
                .warnings(warnings)
                .build();
    }

    // =========================
    // PRICING
    // =========================
    @Cacheable(
            value = "pricing-preview",
            key = "T(com.IntegrityTechnologies.business_manager.modules.finance.sales.sellable.cache.SellableCacheKey)" +
                    ".pricing(#variant.id, #request.branchId, #request.customerId, #request.customerGroupId, #quantity)"
    )
    public PricingPreviewDTO buildPricing(
            ProductVariant variant,
            SellableProductRequest request,
            long quantity
    ) {
        PricingPolicy policy = PricingPolicy.builder()
                .enforceMinimumPrice(false)
                .build();

        PricingContext ctx = PricingContext.builder()
                .tenantId(TenantContext.getTenantId())
                .branchId(request.getBranchId())
                .productVariantId(variant.getId())
                .packagingId(null)
                .quantity(quantity)
                .customerId(request.getCustomerId())
                .customerGroupId(request.getCustomerGroupId())
                .pricingTime(java.time.LocalDateTime.now())
                .policy(policy)
                .build();

        PricingResult result = pricingEngineService.resolve(ctx);

        BigDecimal unitPrice = result.getFinalPrice();

        return PricingPreviewDTO.builder()
                .unitPrice(unitPrice)
                .totalPrice(unitPrice.multiply(BigDecimal.valueOf(quantity)))
                .adjustments(result.getAdjustments())
                .build();
    }

    // =========================
    // BATCH PREVIEW (LIGHT)
    // =========================
    private List<BatchPreviewDTO> buildBatchPreview(UUID variantId, UUID branchId) {

        List<InventoryBatch> batches =
                batchRepository.findAvailableBatches(
                        variantId,
                        TenantContext.getTenantId(),
                        branchId
                );

        return batches.stream()
                .limit(5) // 🔥 only top 5 for UI
                .map(b -> BatchPreviewDTO.builder()
                        .batchId(b.getId())
                        .available(b.getQuantityRemaining())
                        .unitCost(b.getUnitCost())
                        .sellingPrice(b.getUnitSellingPrice())
                        .receivedAt(b.getReceivedAt())
                        .build()
                )
                .toList();
    }

    // =========================
    // WARNINGS ENGINE
    // =========================
    private List<WarningDTO> buildWarnings(
            long available,
            PricingPreviewDTO pricing,
            ProductVariant variant
    ) {

        List<WarningDTO> warnings = new ArrayList<>();

        if (available <= 0) {
            warnings.add(WarningDTO.builder()
                    .type("OUT_OF_STOCK")
                    .message("Item is out of stock")
                    .build());
        } else if (available > 0 && available <= 5) {
            warnings.add(WarningDTO.builder()
                    .type("LOW_STOCK")
                    .message("Low stock remaining")
                    .build());
        }

        if (pricing != null && variant.getMinimumSellingPrice() != null) {
            if (pricing.getUnitPrice().compareTo(variant.getMinimumSellingPrice()) < 0) {
                warnings.add(WarningDTO.builder()
                        .type("BELOW_MIN_PRICE")
                        .message("Price below minimum selling price")
                        .build());
            }
        }

        return warnings;
    }
}