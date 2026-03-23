package com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository.projection;

import java.util.UUID;

public interface BatchReservationSummary {

    UUID getBatchId();
    Long getTotalReserved();
}