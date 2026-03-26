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

    private final FifoValuationStrategy fifo;
    private final LifoValuationStrategy lifo;
    private final WacValuationStrategy wac;

    public BigDecimal valuate(
            String method,
            UUID variantId,
            UUID branchId,
            long qty
    ) {

        switch (method.toUpperCase()) {
            case "FIFO":
                return fifo.valuate(variantId, branchId, qty);

            case "LIFO":
                return lifo.valuate(variantId, branchId, qty);

            case "WAC":
                return wac.valuate(variantId, branchId, qty);

            default:
                throw new IllegalArgumentException("Unsupported valuation method: " + method);
        }
    }
}