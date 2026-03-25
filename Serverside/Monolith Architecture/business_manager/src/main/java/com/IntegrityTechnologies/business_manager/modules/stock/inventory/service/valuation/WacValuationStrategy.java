package com.IntegrityTechnologies.business_manager.modules.stock.inventory.service.valuation;

import com.IntegrityTechnologies.business_manager.modules.stock.inventory.model.ValuationStrategy;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository.InventoryItemRepository;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;
import java.util.UUID;

@Component
@RequiredArgsConstructor
public class WacValuationStrategy implements ValuationStrategy {

    private final InventoryItemRepository itemRepo;

    @Override
    public BigDecimal valuate(UUID variantId, UUID branchId, long qty) {

        return itemRepo
                .findByProductVariantIdAndTenantIdAndBranchIdAndDeletedFalse(
                        variantId,
                        TenantContext.getTenantId(),
                        branchId
                )
                .map(i ->
                        i.getAverageCost()
                                .multiply(BigDecimal.valueOf(qty))
                )
                .orElse(BigDecimal.ZERO);
    }
}