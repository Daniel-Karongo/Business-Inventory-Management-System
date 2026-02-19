package com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository;

import com.IntegrityTechnologies.business_manager.modules.stock.inventory.model.InventoryItem;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface InventoryItemRepository extends JpaRepository<InventoryItem, UUID> {

    Optional<InventoryItem> findByProductVariant_IdAndBranchId(UUID variantId, UUID branchId);

    List<InventoryItem> findByProductVariant_Id(UUID variantId);
    boolean existsByProductVariant_Id(UUID variantId);

    List<InventoryItem> findByBranchId(UUID branchId);

    List<InventoryItem> findByProductVariant_Product_Id(UUID productId);
    Optional<InventoryItem> findFirstByProductVariantId(UUID productVariantId);
    void deleteAllByProductId(UUID productId);
    boolean existsByProductId(UUID id);
    @Transactional
    @Modifying
    @Query("""
        update InventoryItem i
        set i.deleted = true
        where i.productVariant.id in :variantIds
    """)
    void softDeleteByVariantIds(@Param("variantIds") List<UUID> variantIds);

    @Transactional
    @Modifying
    @Query("""
        update InventoryItem i
        set i.deleted = false
        where i.productVariant.id in :variantIds
    """)
    void restoreByVariantIds(@Param("variantIds") List<UUID> variantIds);
}
