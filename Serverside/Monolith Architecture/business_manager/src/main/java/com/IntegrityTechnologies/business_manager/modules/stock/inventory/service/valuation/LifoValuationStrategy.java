package com.IntegrityTechnologies.business_manager.modules.stock.inventory.service.valuation;

import com.IntegrityTechnologies.business_manager.modules.stock.inventory.model.InventoryBatch;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.model.ValuationStrategy;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository.InventoryBatchRepository;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;
import java.util.Collections;
import java.util.List;
import java.util.UUID;

@Component
@RequiredArgsConstructor
public class LifoValuationStrategy implements ValuationStrategy {

    private final InventoryBatchRepository batchRepository;

    @Override
    public BigDecimal valuate(UUID variantId, UUID branchId, long qty) {

        List<InventoryBatch> batches =
                batchRepository.findAvailableBatches(
                        variantId,
                        TenantContext.getTenantId(),
                        branchId
                );

        Collections.reverse(batches); // 🔥 KEY FIX

        BigDecimal total = BigDecimal.ZERO;
        long remaining = qty;

        for (InventoryBatch b : batches) {

            if (remaining <= 0) break;

            long used = Math.min(b.getQuantityRemaining(), remaining);

            total = total.add(
                    b.getUnitCost().multiply(BigDecimal.valueOf(used))
            );

            remaining -= used;
        }

        return total;
    }
}