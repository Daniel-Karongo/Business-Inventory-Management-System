package com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository;

import com.IntegrityTechnologies.business_manager.modules.stock.inventory.model.InventoryItem;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface InventoryItemRepository extends JpaRepository<InventoryItem, UUID> {

    Optional<InventoryItem> findByProductVariant_IdAndBranchId(UUID variantId, UUID branchId);

    List<InventoryItem> findByProductVariant_Id(UUID variantId);

    List<InventoryItem> findByBranchId(UUID branchId);

    List<InventoryItem> findByProductVariant_Product_Id(UUID productId);
    Optional<InventoryItem> findFirstByProductVariantId(UUID productVariantId);
}
