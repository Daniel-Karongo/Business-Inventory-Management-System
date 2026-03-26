package com.IntegrityTechnologies.business_manager.modules.stock.inventory.engine;

import com.IntegrityTechnologies.business_manager.modules.stock.inventory.model.*;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository.*;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class StockConsumptionService {

    private final BatchReservationRepository reservationRepo;
    private final InventoryBatchRepository batchRepo;
    private final BatchConsumptionRepository batchConsumptionRepository;
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

        for (BatchReservation r : reservations) {

            if (r.getStatus() != ReservationStatus.ACTIVE) continue;

            InventoryBatch batch = r.getBatch();

            long remaining = batch.getQuantityRemaining() - r.getQuantity();

            if (remaining < 0) {
                throw new IllegalStateException("Batch underflow");
            }

            batch.setQuantityRemaining(remaining);
            batchRepo.save(batch);

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

            r.setStatus(ReservationStatus.CONSUMED);
            reservationRepo.save(r);
        }
    }

    @Transactional
    public void consumeAndSync(
            UUID variantId,
            UUID branchId,
            UUID referenceId
    ) {

        consume(branchId, referenceId);

        long consumed =
                batchConsumptionRepository.sumQuantityBySaleId(referenceId, tenantId());

        updateItem(variantId, branchId, -consumed);
    }

    private void updateItem(UUID variantId, UUID branchId, long delta) {

        InventoryItem item =
                inventoryItemRepository.lockByVariant(variantId, tenantId(), branchId)
                        .orElseThrow(() -> new IllegalStateException("Inventory item missing"));

        item.setQuantityOnHand(item.getQuantityOnHand() + delta);
        item.setLastUpdatedAt(LocalDateTime.now());

        inventoryItemRepository.save(item);
    }
}