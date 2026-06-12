package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.service;

import com.IntegrityTechnologies.business_manager.modules.stock.inventory.model.InventoryItem;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository.InventoryItemRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class InventoryCostService {

    private final InventoryItemRepository inventoryItemRepository;

    public BigDecimal getAverageCost(
            UUID tenantId,
            UUID variantId,
            UUID branchId
    ) {
        return inventoryItemRepository
                .findByProductVariantIdAndTenantIdAndBranchIdAndDeletedFalse(
                        variantId,
                        tenantId,
                        branchId
                )
                .map(InventoryItem::getAverageCost)
                .orElse(BigDecimal.ZERO);
    }
}