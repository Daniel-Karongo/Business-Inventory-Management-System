package com.IntegrityTechnologies.business_manager.modules.stock.inventory.engine;

import com.IntegrityTechnologies.business_manager.modules.stock.inventory.accounting.InventoryAccountingPort;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.model.BatchConsumption;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.model.BatchReservation;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.model.InventoryBatch;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.model.ReservationStatus;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository.BatchConsumptionRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository.BatchReservationRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository.InventoryBatchRepository;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class StockConsumptionService {

    private final BatchReservationRepository reservationRepo;
    private final InventoryBatchRepository batchRepo;
    private final BatchConsumptionRepository batchConsumptionRepository;
    private final InventoryAccountingPort inventoryAccountingPort;

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

        for (BatchReservation r : reservations) {

            if (r.getStatus() != ReservationStatus.ACTIVE) {
                continue;
            }

            InventoryBatch batch = r.getBatch();

            long remaining =
                    batch.getQuantityRemaining() - r.getQuantity();

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

            BigDecimal lineCost =
                    batch.getUnitCost()
                            .multiply(
                                    BigDecimal.valueOf(
                                            r.getQuantity()
                                    )
                            );

            totalCost = totalCost.add(lineCost);

            r.setStatus(ReservationStatus.CONSUMED);

            reservationRepo.save(r);
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
}