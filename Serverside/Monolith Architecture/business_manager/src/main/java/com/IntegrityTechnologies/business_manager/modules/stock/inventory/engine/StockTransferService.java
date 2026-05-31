package com.IntegrityTechnologies.business_manager.modules.stock.inventory.engine;

import com.IntegrityTechnologies.business_manager.modules.stock.inventory.accounting.InventoryAccountingPort;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.model.BatchConsumption;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.model.InventoryBatch;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.model.InventoryItem;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository.BatchConsumptionRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository.BatchReservationRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository.InventoryBatchRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository.InventoryItemRepository;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class StockTransferService {

    private final InventoryBatchRepository batchRepo;
    private final BatchReservationRepository reservationRepo;
    private final BatchConsumptionRepository batchConsumptionRepository;
    private final InventoryItemRepository inventoryItemRepository;
    private final InventoryAccountingPort inventoryAccountingPort;

    private UUID tenantId() {
        return TenantContext.getTenantId();
    }

    @Transactional
    public BigDecimal transfer(
            UUID variantId,
            UUID fromBranch,
            UUID toBranch,
            long quantity,
            BigDecimal overrideCost
    ) {

        List<InventoryBatch> batches =
                batchRepo.lockBatchesByBranch(variantId, tenantId(), fromBranch);

        Map<UUID, Long> reservedMap =
                reservationRepo.sumReservedByBatch(variantId, tenantId(), fromBranch)
                        .stream()
                        .collect(Collectors.toMap(r -> r.getBatchId(), r -> r.getTotalReserved()));

        long remaining = quantity;
        BigDecimal totalValue = BigDecimal.ZERO;

        for (InventoryBatch batch : batches) {

            if (remaining <= 0) break;

            long available =
                    batch.getQuantityRemaining() - reservedMap.getOrDefault(batch.getId(), 0L);

            long move = Math.min(available, remaining);
            if (move <= 0) continue;

            batch.setQuantityRemaining(batch.getQuantityRemaining() - move);

            BigDecimal cost =
                    batch.getUnitCost().multiply(BigDecimal.valueOf(move));

            totalValue = totalValue.add(cost);

            batchRepo.save(batch);

            batchRepo.save(
                    InventoryBatch.builder()
                            .productVariantId(variantId)
                            .branchId(toBranch)
                            .tenantId(tenantId())
                            .unitCost(overrideCost != null ? overrideCost : batch.getUnitCost())
                            .quantityReceived(move)
                            .quantityRemaining(move)
                            .receivedAt(batch.getReceivedAt())
                            .build()
            );

            remaining -= move;
        }

        if (remaining > 0) {
            throw new IllegalStateException("Insufficient stock");
        }

        return totalValue;
    }

    @Transactional
    public BigDecimal transferAndSync(
            UUID variantId,
            UUID fromBranch,
            UUID toBranch,
            long quantity,
            BigDecimal overrideCost,
            UUID transferId,
            String reference
    ) {

        BigDecimal value =
                transfer(
                        variantId,
                        fromBranch,
                        toBranch,
                        quantity,
                        overrideCost
                );

        updateItem(
                variantId,
                fromBranch,
                -quantity
        );

        updateItem(
                variantId,
                toBranch,
                +quantity
        );

        validateInvariant(
                variantId,
                fromBranch
        );

        validateInvariant(
                variantId,
                toBranch
        );

        inventoryAccountingPort.recordInventoryTransferOut(
                tenantId(),
                transferId,
                fromBranch,
                value,
                reference
        );

        inventoryAccountingPort.recordInventoryTransferIn(
                tenantId(),
                transferId,
                toBranch,
                value,
                reference
        );

        return value;
    }

    private void updateItem(UUID variantId, UUID branchId, long delta) {

        InventoryItem item =
                inventoryItemRepository.lockByVariant(variantId, tenantId(), branchId)
                        .orElseThrow();

        item.setQuantityOnHand(item.getQuantityOnHand() + delta);

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