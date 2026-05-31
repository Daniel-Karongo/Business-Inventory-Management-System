package com.IntegrityTechnologies.business_manager.modules.stock.inventory.engine;

import com.IntegrityTechnologies.business_manager.modules.stock.inventory.accounting.InventoryAccountingPort;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.model.*;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository.BatchConsumptionRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository.BatchReservationRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository.InventoryBatchRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository.InventoryItemRepository;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class StockConsumptionService {

    private final BatchReservationRepository reservationRepo;
    private final InventoryBatchRepository batchRepo;
    private final BatchConsumptionRepository batchConsumptionRepository;
    private final InventoryAccountingPort inventoryAccountingPort;
    private final InventoryItemRepository inventoryItemRepository;

    private UUID tenantId() {
        return TenantContext.getTenantId();
    }

    @Transactional
    public void consume(UUID branchId, UUID referenceId) {

        List<BatchReservation> reservations =
                reservationRepo.lockByReferenceIdAndTenantIdAndBranchId(
                        referenceId,
                        tenantId(),
                        branchId
                );

        BigDecimal totalCost = BigDecimal.ZERO;

        Set<UUID> affectedVariants =
                new HashSet<>();

        for (BatchReservation r : reservations) {

            if (r.getExpiresAt() != null
                    && r.getExpiresAt().isBefore(LocalDateTime.now()))
            {
                r.setStatus(ReservationStatus.EXPIRED);
                reservationRepo.save(r);
                continue;
            }

            if (r.getStatus() != ReservationStatus.ACTIVE) {
                continue;
            }

            InventoryBatch batch =
                    batchRepo.findByIdForUpdate(
                                    r.getBatch().getId(),
                                    tenantId(),
                                    branchId
                            )
                            .orElseThrow();

            long remaining =
                    batch.getQuantityRemaining() - r.getQuantity();

            if (remaining < 0) {
                throw new IllegalStateException("Batch underflow");
            }

            batch.setQuantityRemaining(remaining);

            batchRepo.save(batch);

            updateInventoryItem(
                    r.getProductVariantId(),
                    branchId,
                    r.getQuantity()
            );

            batchConsumptionRepository.save(
                    BatchConsumption.builder()
                            .batch(batch)
                            .saleId(r.getSaleId())
                            .productVariantId(r.getProductVariantId())
                            .quantity(r.getQuantity())
                            .unitCost(batch.getUnitCost())
                            .reservationId(r.getId())
                            .tenantId(tenantId())
                            .branchId(branchId)
                            .build()
            );

            BigDecimal lineCost =
                    batch.getUnitCost()
                            .multiply(
                                    BigDecimal.valueOf(
                                            r.getQuantity()
                                    )
                            );

            totalCost = totalCost.add(lineCost);

            r.setStatus(ReservationStatus.CONSUMED);
            affectedVariants.add(
                    r.getProductVariantId()
            );

            reservationRepo.save(r);
        }

        for (UUID variantId : affectedVariants) {

            validateInvariant(
                    variantId,
                    branchId
            );
        }

        if (totalCost.compareTo(BigDecimal.ZERO) > 0) {

            inventoryAccountingPort.recordInventoryConsumption(
                    tenantId(),
                    referenceId,
                    branchId,
                    totalCost,
                    "SALE_DELIVERY:" + referenceId
            );
        }
    }

    private void updateInventoryItem(
            UUID variantId,
            UUID branchId,
            long quantity
    ) {
        InventoryItem item =
                inventoryItemRepository
                        .lockByVariant(
                                variantId,
                                tenantId(),
                                branchId
                        )
                        .orElseThrow();

        item.setQuantityOnHand(
                item.getQuantityOnHand() - quantity
        );

        item.setLastUpdatedAt(
                LocalDateTime.now()
        );

        inventoryItemRepository.save(item);
    }

    private void validateInvariant(
            UUID variantId,
            UUID branchId
    ) {

        InventoryItem item =
                inventoryItemRepository.findByProductVariantIdAndTenantIdAndBranchIdAndDeletedFalse(
                                variantId,
                                tenantId(),
                                branchId
                        )
                        .orElseThrow();

        long batchSum =
                batchRepo.sumRemainingByVariant(
                        variantId,
                        tenantId(),
                        branchId
                );

        if (item.getQuantityOnHand() != batchSum) {

            throw new IllegalStateException(
                    "Inventory invariant violated"
            );
        }
    }
}