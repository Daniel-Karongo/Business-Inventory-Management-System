package com.IntegrityTechnologies.business_manager.modules.stock.inventory.accounting;

import java.math.BigDecimal;
import java.util.UUID;

public interface InventoryAccountingPort {

    void recordInventoryReceipt(
            UUID referenceId,
            UUID branchId,
            BigDecimal value,
            String reference
    );

    void recordInventoryConsumption(
            UUID referenceId,
            UUID branchId,
            BigDecimal value,
            String reference
    );

    void recordInventoryReturn(
            UUID referenceId,
            UUID branchId,
            BigDecimal value,
            String reference
    );

    /** ===========================
     * TRANSFERS (NEW)
     * =========================== */

    void recordInventoryTransferOut(
            UUID referenceId,
            UUID branchId,
            BigDecimal value,
            String reference
    );

    void recordInventoryTransferIn(
            UUID referenceId,
            UUID branchId,
            BigDecimal value,
            String reference
    );
}