package com.IntegrityTechnologies.business_manager.modules.inventory.repository;

import com.IntegrityTechnologies.business_manager.modules.inventory.model.InventoryItem;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.Optional;
import java.util.UUID;

@Repository
public interface InventoryItemRepository extends JpaRepository<InventoryItem, UUID> {
    Optional<InventoryItem> findByProductId(UUID productId);
}
