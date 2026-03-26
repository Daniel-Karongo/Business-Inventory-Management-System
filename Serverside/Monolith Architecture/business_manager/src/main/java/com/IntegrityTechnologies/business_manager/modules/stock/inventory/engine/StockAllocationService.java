package com.IntegrityTechnologies.business_manager.modules.stock.inventory.engine;

import com.IntegrityTechnologies.business_manager.exception.OutOfStockException;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.sellable.dto.AllocationDetail;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.sellable.dto.AllocationResult;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.model.InventoryBatch;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository.BatchReservationRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository.InventoryBatchRepository;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.util.*;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class StockAllocationService {

    private final InventoryBatchRepository batchRepo;
    private final BatchReservationRepository reservationRepo;

    private UUID tenantId() {
        return TenantContext.getTenantId();
    }

    public List<InventoryBatch> allocate(
            UUID variantId,
            UUID branchId,
            long requiredBaseUnits
    ) {

        List<InventoryBatch> batches =
                batchRepo.findAvailableBatches(variantId, tenantId(), branchId);

        Map<UUID, Long> reservedMap = reservedPerBatch(variantId, branchId);

        List<InventoryBatch> selected = new ArrayList<>();
        long remaining = requiredBaseUnits;

        for (InventoryBatch b : batches) {

            if (remaining <= 0) break;

            long available = available(b, reservedMap);
            if (available <= 0) continue;

            selected.add(b);
            remaining -= Math.min(available, remaining);
        }

        if (remaining > 0) {
            throw new OutOfStockException("Insufficient stock");
        }

        return selected;
    }

    public AllocationResult previewAllocation(
            UUID variantId,
            UUID branchId,
            long quantity,
            List<UUID> selectedBatchIds
    ) {

        Map<UUID, Long> reservedMap = reservedPerBatch(variantId, branchId);

        long remaining = quantity;
        BigDecimal totalCost = BigDecimal.ZERO;

        List<AllocationDetail> rows = new ArrayList<>();

        List<InventoryBatch> batches =
                batchRepo.findAvailableBatches(
                        variantId,
                        tenantId(),
                        branchId
                );

        Map<UUID, InventoryBatch> batchMap =
                batches.stream().collect(Collectors.toMap(InventoryBatch::getId, b -> b));

        // MANUAL FIRST
        if (selectedBatchIds != null) {
            for (UUID id : selectedBatchIds) {

                InventoryBatch batch = batchMap.get(id);
                if (batch == null) continue;

                long available = available(batch, reservedMap);
                if (available <= 0) continue;

                long allocate = Math.min(available, remaining);

                BigDecimal cost =
                        batch.getUnitCost().multiply(BigDecimal.valueOf(allocate));

                rows.add(buildDetail(batch, allocate, available,
                        reservedMap.getOrDefault(batch.getId(), 0L), cost));

                totalCost = totalCost.add(cost);
                remaining -= allocate;

                if (remaining <= 0) break;
            }
        }

        // FIFO
        for (InventoryBatch batch : batches) {

            if (remaining <= 0) break;
            if (selectedBatchIds != null && selectedBatchIds.contains(batch.getId()))
                continue;

            long available = available(batch, reservedMap);
            if (available <= 0) continue;

            long allocate = Math.min(available, remaining);

            BigDecimal cost =
                    batch.getUnitCost().multiply(BigDecimal.valueOf(allocate));

            rows.add(buildDetail(batch, allocate, available,
                    reservedMap.getOrDefault(batch.getId(), 0L), cost));

            totalCost = totalCost.add(cost);
            remaining -= allocate;
        }

        if (remaining > 0)
            throw new OutOfStockException("Insufficient stock");

        return AllocationResult.builder()
                .allocations(rows)
                .totalCost(totalCost)
                .build();
    }

    private AllocationDetail buildDetail(
            InventoryBatch batch,
            long allocated,
            long available,
            long reserved,
            BigDecimal cost
    ) {
        return AllocationDetail.builder()
                .batchId(batch.getId())
                .allocatedQuantity(allocated)
                .availableQuantity(available)
                .reservedQuantity(reserved)
                .unitCost(batch.getUnitCost())
                .totalCost(cost)
                .receivedAt(batch.getReceivedAt())
                .build();
    }

    public long availableQuantity(UUID variantId, UUID branchId) {

        Map<UUID, Long> reservedMap = reservedPerBatch(variantId, branchId);

        return batchRepo.findAvailableBatches(variantId, tenantId(), branchId)
                .stream()
                .mapToLong(b -> Math.max(0, available(b, reservedMap)))
                .sum();
    }

    public long reservedQuantity(UUID variantId, UUID branchId) {
        return reservationRepo.sumReservedByVariantAndTenantAndBranch(
                variantId,
                tenantId(),
                branchId
        );
    }

    public Map<UUID, Long> reservedPerBatch(UUID variantId, UUID branchId) {
        return reservationRepo.sumReservedByBatch(
                variantId,
                tenantId(),
                branchId
        ).stream().collect(Collectors.toMap(
                r -> r.getBatchId(),
                r -> r.getTotalReserved()
        ));
    }

    private long available(InventoryBatch b, Map<UUID, Long> reservedMap) {
        return b.getQuantityRemaining() - reservedMap.getOrDefault(b.getId(), 0L);
    }
}