package com.IntegrityTechnologies.business_manager.modules.stock.inventory.accounting;

import java.math.BigDecimal;
import java.util.UUID;

public interface InventoryAccountingPort {

    void recordInventoryReceipt(
            UUID tenantId,
            UUID referenceId,
            UUID branchId,
            BigDecimal value,
            String reference
    );

    void recordInventoryConsumption(
            UUID tenantId,
            UUID referenceId,
            UUID branchId,
            BigDecimal value,
            String reference
    );

    void recordInventoryReturn(
            UUID tenantId,
            UUID referenceId,
            UUID branchId,
            BigDecimal value,
            String reference
    );

    void recordInventoryTransferOut(
            UUID tenantId,
            UUID referenceId,
            UUID branchId,
            BigDecimal value,
            String reference
    );

    void recordInventoryTransferIn(
            UUID tenantId,
            UUID referenceId,
            UUID branchId,
            BigDecimal value,
            String reference
    );
}