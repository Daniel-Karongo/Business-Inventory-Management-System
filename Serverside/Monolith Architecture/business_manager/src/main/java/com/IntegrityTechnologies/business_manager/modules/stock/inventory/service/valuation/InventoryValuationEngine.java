package com.IntegrityTechnologies.business_manager.modules.stock.inventory.service.valuation;

import com.IntegrityTechnologies.business_manager.modules.stock.inventory.model.ValuationStrategy;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.util.Map;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class InventoryValuationEngine {

    private final Map<String, ValuationStrategy> strategies;

    public BigDecimal valuate(
            String method,
            UUID variantId,
            UUID branchId,
            long qty
    ) {
        ValuationStrategy strategy = strategies.get(method);

        if (strategy == null) {
            throw new IllegalArgumentException("Unsupported valuation method");
        }

        return strategy.valuate(variantId, branchId, qty);
    }
}