package com.IntegrityTechnologies.business_manager.modules.stock.inventory.engine;

import com.IntegrityTechnologies.business_manager.exception.OutOfStockException;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.base.model.SaleLineBatchSelection;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.model.*;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository.BatchReservationRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository.InventoryBatchRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository.TenantInventorySettingsRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.packaging.model.ProductVariantPackaging;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.packaging.repository.ProductVariantPackagingRepository;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class StockReservationService {

    private final StockAllocationService allocationService;
    private final BatchReservationRepository reservationRepo;
    private final InventoryBatchRepository batchRepo;
    private final ProductVariantPackagingRepository packagingRepo;
    private final TenantInventorySettingsRepository settingsRepo;

    private UUID tenantId() {
        return TenantContext.getTenantId();
    }

    /* =====================================================
       BASE RESERVE (FULL PARITY)
    ===================================================== */

    @Transactional
    public List<BatchReservation> reserve(
            UUID saleId,
            Long saleLineItemId,
            UUID variantId,
            UUID packagingId,
            UUID branchId,
            long baseUnits
    ) {

        ProductVariantPackaging packaging =
                packagingId != null
                        ? packagingRepo.findById(packagingId)
                        .orElseThrow(() -> new IllegalArgumentException("Packaging not found"))
                        : null;

        TenantInventorySettings settings =
                settingsRepo.findById(tenantId()).orElse(null);

        enforcePackaging(packaging, settings, baseUnits);

        List<InventoryBatch> batches =
                allocationService.allocate(variantId, branchId, baseUnits);

        Map<UUID, Long> reservedMap = reservedPerBatch(variantId, branchId);

        List<BatchReservation> reservations = new ArrayList<>();
        long remaining = baseUnits;

        for (InventoryBatch batch : batches) {

            if (remaining <= 0) break;

            long available = available(batch, reservedMap);
            long take = Math.min(available, remaining);

            if (take <= 0) continue;

            reservations.add(buildReservation(
                    batch,
                    variantId,
                    saleId,
                    saleLineItemId,
                    packagingId,
                    baseUnits,
                    packaging,
                    settings,
                    take
            ));

            remaining -= take;
        }

        if (remaining > 0) throw new OutOfStockException("Insufficient stock");

        return reservations;
    }

    /* =====================================================
       FULL EXTRACTION — reserveWithSelection
    ===================================================== */

    @Transactional
    public List<BatchReservation> reserveWithSelection(
            UUID saleId,
            UUID variantId,
            UUID packagingId,
            UUID branchId,
            long baseUnits,
            long requestedQuantity,
            List<SaleLineBatchSelection> selections
    ) {

        ProductVariantPackaging packaging =
                packagingId != null
                        ? packagingRepo.findById(packagingId)
                        .orElseThrow(() -> new IllegalArgumentException("Packaging not found"))
                        : null;

        TenantInventorySettings settings =
                settingsRepo.findById(tenantId()).orElse(null);

        enforcePackaging(packaging, settings, baseUnits);

        List<InventoryBatch> batches =
                batchRepo.findAvailableBatches(variantId, tenantId(), branchId);

        Map<UUID, InventoryBatch> batchMap =
                batches.stream().collect(Collectors.toMap(InventoryBatch::getId, b -> b));

        Map<UUID, Long> reservedMap = reservedPerBatch(variantId, branchId);

        long remaining = baseUnits;
        List<BatchReservation> reservations = new ArrayList<>();

        // ---------- MANUAL ----------
        if (selections != null) {
            for (SaleLineBatchSelection sel : selections) {

                InventoryBatch batch = batchMap.get(sel.getBatchId());
                if (batch == null) continue;

                long available = available(batch, reservedMap);
                long take = Math.min(sel.getQuantity(), available);

                if (take <= 0) continue;

                reservations.add(
                        buildReservation(
                                batch,
                                variantId,
                                saleId,
                                sel.getSaleLineItem().getId(),
                                packagingId,
                                baseUnits,
                                packaging,
                                settings,
                                take
                        )
                );

                remaining -= take;
                if (remaining <= 0) break;
            }
        }

        // ---------- FIFO ----------
        if (remaining > 0) {
            List<InventoryBatch> fifo =
                    allocationService.allocate(variantId, branchId, remaining);

            for (InventoryBatch batch : fifo) {

                long available = available(batch, reservedMap);
                long take = Math.min(available, remaining);

                if (take <= 0) continue;

                reservations.add(
                        buildReservation(
                                batch,
                                variantId,
                                saleId,
                                null,
                                packagingId,
                                baseUnits,
                                packaging,
                                settings,
                                take
                        )
                );

                remaining -= take;
                if (remaining <= 0) break;
            }
        }

        if (remaining > 0) {
            throw new OutOfStockException("Insufficient stock");
        }

        return reservations;
    }

    /* =====================================================
       LOW LEVEL (EXTRACTED)
    ===================================================== */

    @Transactional
    public BatchReservation reserveFromBatch(
            InventoryBatch batch,
            UUID variantId,
            UUID saleId,
            long qty
    ) {
        return reservationRepo.save(
                BatchReservation.builder()
                        .batch(batch)
                        .productVariantId(variantId)
                        .quantity(qty)
                        .saleId(saleId)
                        .referenceId(saleId)
                        .status(ReservationStatus.ACTIVE)
                        .tenantId(tenantId())
                        .branchId(batch.getBranchId())
                        .build()
        );
    }

    /* =====================================================
       RELEASE
    ===================================================== */

    @Transactional
    public void release(UUID branchId, UUID saleId) {

        List<BatchReservation> reservations =
                reservationRepo.findByReferenceIdAndTenantIdAndBranchId(
                        saleId,
                        tenantId(),
                        branchId
                );

        for (BatchReservation r : reservations) {
            r.setStatus(ReservationStatus.RELEASED);
            reservationRepo.save(r);
        }
    }

    /* =====================================================
       HELPERS
    ===================================================== */

    private void enforcePackaging(
            ProductVariantPackaging packaging,
            TenantInventorySettings settings,
            long baseUnits
    ) {
        if (packaging != null
                && settings != null
                && Boolean.TRUE.equals(settings.getEnforcePackagingIntegrity())
                && !Boolean.TRUE.equals(settings.getAllowPartialPackaging())) {

            long units = packaging.getUnitsPerPackaging();

            if (baseUnits % units != 0) {
                throw new IllegalStateException("Cannot break packaging");
            }
        }
    }

    private Map<UUID, Long> reservedPerBatch(UUID variantId, UUID branchId) {
        return reservationRepo.sumReservedByBatch(
                variantId,
                tenantId(),
                branchId
        ).stream().collect(Collectors.toMap(
                r -> r.getBatchId(),
                r -> r.getTotalReserved()
        ));
    }

    private long available(InventoryBatch batch, Map<UUID, Long> reservedMap) {
        return batch.getQuantityRemaining() - reservedMap.getOrDefault(batch.getId(), 0L);
    }

    private BatchReservation buildReservation(
            InventoryBatch batch,
            UUID variantId,
            UUID saleId,
            Long saleLineItemId,
            UUID packagingId,
            long baseUnits,
            ProductVariantPackaging packaging,
            TenantInventorySettings settings,
            long take
    ) {

        return reservationRepo.save(
                BatchReservation.builder()
                        .batch(batch)
                        .productVariantId(variantId)
                        .quantity(take)
                        .saleId(saleId)
                        .saleLineItemId(saleLineItemId)
                        .packagingId(packagingId)
                        .requestedQuantity(
                                packaging != null
                                        ? baseUnits / packaging.getUnitsPerPackaging()
                                        : baseUnits
                        )
                        .status(ReservationStatus.ACTIVE)
                        .expiresAt(
                                settings != null && settings.getReservationTtlMinutes() != null
                                        ? LocalDateTime.now().plusMinutes(settings.getReservationTtlMinutes())
                                        : null
                        )
                        .referenceId(saleId)
                        .tenantId(tenantId())
                        .branchId(batch.getBranchId())
                        .build()
        );
    }
}