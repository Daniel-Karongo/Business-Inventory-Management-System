package com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository;

import com.IntegrityTechnologies.business_manager.modules.stock.inventory.model.InventoryBatch;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.UUID;

public interface InventoryBatchRepository
        extends JpaRepository<InventoryBatch, UUID> {

    List<InventoryBatch> findByProductVariantIdAndBranchIdAndQuantityRemainingGreaterThanOrderByReceivedAtAsc(
            UUID variantId,
            UUID branchId,
            Long minRemaining
    );
}