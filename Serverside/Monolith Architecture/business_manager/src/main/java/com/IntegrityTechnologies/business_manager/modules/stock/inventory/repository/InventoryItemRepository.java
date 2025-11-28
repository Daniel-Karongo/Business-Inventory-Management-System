package com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository;

import com.IntegrityTechnologies.business_manager.modules.stock.inventory.model.InventoryItem;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface InventoryItemRepository extends JpaRepository<InventoryItem, UUID> {

    Optional<InventoryItem> findByProductIdAndBranchId(UUID productId, UUID branchId);

    List<InventoryItem> findByBranchId(UUID branchId);

    List<InventoryItem> findByProductId(UUID productId);
}