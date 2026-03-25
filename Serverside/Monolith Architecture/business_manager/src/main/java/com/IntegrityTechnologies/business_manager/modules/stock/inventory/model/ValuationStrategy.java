package com.IntegrityTechnologies.business_manager.modules.stock.inventory.model;

import java.math.BigDecimal;
import java.util.UUID;

public interface ValuationStrategy {

    BigDecimal valuate(
            UUID variantId,
            UUID branchId,
            long quantity
    );
}
