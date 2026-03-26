package com.IntegrityTechnologies.business_manager.modules.stock.inventory.engine;

import com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository.BatchConsumptionRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository.InventoryBatchRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository.InventoryItemRepository;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class StockAdjustmentService {

    private final InventoryBatchRepository batchRepo;
    private final BatchConsumptionRepository batchConsumptionRepository;
    private final InventoryItemRepository inventoryItemRepository;
    private final StockReservationService reservationService;
    private final StockConsumptionService consumptionService;

    private UUID tenantId() {
        return TenantContext.getTenantId();
    }

    @Transactional
    public BigDecimal adjustAndSync(
            UUID variantId,
            UUID branchId,
            long delta,
            BigDecimal unitCost,
            UUID referenceId
    ) {

        if (delta < 0) {

            long qty = Math.abs(delta);

            reservationService.reserve(referenceId, null, variantId, null, branchId, qty);
            consumptionService.consume(branchId, referenceId);

            BigDecimal totalCost =
                    batchConsumptionRepository.sumCostBySaleId(referenceId, tenantId());

            updateItem(variantId, branchId, -qty);

            return totalCost;
        }

        long qty = delta;

        var batch = batchRepo.save(
                com.IntegrityTechnologies.business_manager.modules.stock.inventory.model.InventoryBatch.builder()
                        .productVariantId(variantId)
                        .branchId(branchId)
                        .unitCost(unitCost != null ? unitCost : BigDecimal.ZERO)
                        .quantityReceived(qty)
                        .quantityRemaining(qty)
                        .receivedAt(LocalDateTime.now())
                        .build()
        );

        updateItem(variantId, branchId, qty);

        return batch.getUnitCost().multiply(BigDecimal.valueOf(qty));
    }

    private void updateItem(UUID variantId, UUID branchId, long delta) {

        var item =
                inventoryItemRepository.lockByVariant(variantId, tenantId(), branchId)
                        .orElseThrow();

        item.setQuantityOnHand(item.getQuantityOnHand() + delta);
        item.setLastUpdatedAt(LocalDateTime.now());

        inventoryItemRepository.save(item);
    }
}