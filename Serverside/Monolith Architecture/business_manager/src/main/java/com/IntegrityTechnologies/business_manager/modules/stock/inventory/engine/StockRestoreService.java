package com.IntegrityTechnologies.business_manager.modules.stock.inventory.engine;

import com.IntegrityTechnologies.business_manager.modules.stock.inventory.model.BatchConsumption;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.model.InventoryBatch;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.model.InventoryItem;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository.BatchConsumptionRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository.InventoryBatchRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository.InventoryItemRepository;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class StockRestoreService {

    private final BatchConsumptionRepository batchConsumptionRepository;
    private final InventoryBatchRepository batchRepo;
    private final InventoryItemRepository inventoryItemRepository;

    private UUID tenantId() {
        return TenantContext.getTenantId();
    }

    @Transactional
    public void restoreAndSync(
            UUID variantId,
            UUID branchId,
            UUID saleId
    ) {

        List<BatchConsumption> consumptions =
                batchConsumptionRepository
                        .findBySaleIdAndProductVariantIdAndTenantIdAndBranchId(
                                saleId,
                                variantId,
                                tenantId(),
                                branchId
                        );

        long totalRestore = 0;

        for (BatchConsumption bc : consumptions) {

            InventoryBatch batch = bc.getBatch();

            long newQty = batch.getQuantityRemaining() + bc.getQuantity();

            if (newQty > batch.getQuantityReceived()) {
                throw new IllegalStateException("Restore overflow");
            }

            batch.setQuantityRemaining(newQty);
            batchRepo.save(batch);

            totalRestore += bc.getQuantity();
        }

        InventoryItem item =
                inventoryItemRepository.lockByVariant(variantId, tenantId(), branchId)
                        .orElseThrow();

        item.setQuantityOnHand(item.getQuantityOnHand() + totalRestore);

        inventoryItemRepository.save(item);
    }
}