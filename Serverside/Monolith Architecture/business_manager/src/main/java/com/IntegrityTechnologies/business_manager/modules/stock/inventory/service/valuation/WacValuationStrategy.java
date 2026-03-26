package com.IntegrityTechnologies.business_manager.modules.stock.inventory.service.valuation;

import com.IntegrityTechnologies.business_manager.modules.stock.inventory.model.ValuationStrategy;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository.InventoryBatchRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository.InventoryItemRepository;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;
import java.util.UUID;

@Component
@RequiredArgsConstructor
public class WacValuationStrategy implements ValuationStrategy {

    private final InventoryBatchRepository batchRepository;

    @Override
    public BigDecimal valuate(UUID variantId, UUID branchId, long qty) {

        Object[] result = batchRepository.computeWeightedAverageRaw(
                variantId,
                TenantContext.getTenantId(),
                branchId
        );

        BigDecimal totalValue = (BigDecimal) result[0];

        return totalValue;
    }
}