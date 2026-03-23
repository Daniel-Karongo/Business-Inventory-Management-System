package com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository.projection;

import java.util.UUID;

public interface VariantReservationSummary {

    UUID getProductVariantId();
    UUID getBranchId();
    Long getTotalReserved();
}