package com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository;

import com.IntegrityTechnologies.business_manager.modules.stock.inventory.model.InventoryItem;
import jakarta.persistence.LockModeType;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Lock;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

@Repository
public interface InventoryItemRepository extends JpaRepository<InventoryItem, UUID> {

    /* =====================================================
       STRICT SCOPED READS
    ===================================================== */

    List<InventoryItem> findByTenantIdAndDeletedFalse(UUID tenantId);

    List<InventoryItem> findByTenantIdAndBranchIdAndDeletedFalse(UUID tenantId, UUID branchId);

    Optional<InventoryItem> findFirstByProductVariantIdAndTenantId(UUID variantId, UUID tenantId);
    Optional<InventoryItem> findByProductVariantIdAndTenantIdAndBranchIdAndDeletedFalse(
            UUID variantId,
            UUID tenantId,
            UUID branchId
    );

    Optional<InventoryItem> findByProductVariantIdAndTenantIdAndBranchId(
            UUID variantId,
            UUID uuid,
            UUID branchId
    );
    boolean existsByProductVariantIdAndTenantIdAndBranchIdAndDeletedFalse(
            UUID variantId,
            UUID tenantId,
            UUID branchId
    );

    boolean existsByProductIdAndTenantIdAndBranchId(
            UUID productId,
            UUID tenantId,
            UUID branchId
    );

    /* =====================================================
       LOCKED READ (CRITICAL FOR CONCURRENCY)
    ===================================================== */

    @Lock(LockModeType.PESSIMISTIC_WRITE)
    @Query("""
        SELECT i FROM InventoryItem i
        WHERE i.productVariantId = :variantId
          AND i.tenantId = :tenantId
          AND i.branchId = :branchId
          AND i.deleted = false
    """)
    Optional<InventoryItem> lockByVariant(
            @Param("variantId") UUID variantId,
            @Param("tenantId") UUID tenantId,
            @Param("branchId") UUID branchId
    );

    /* =====================================================
       PAGINATED QUERIES (MANDATORY AT SCALE)
    ===================================================== */

    @Query("""
    SELECT i FROM InventoryItem i
    WHERE i.tenantId = :tenantId
      AND i.deleted = false
""")
    Page<InventoryItem> findAllActive(UUID tenantId, Pageable pageable);


    @Query("""
    SELECT i FROM InventoryItem i
    WHERE i.tenantId = :tenantId
      AND i.branchId = :branchId
      AND i.deleted = false
""")
    Page<InventoryItem> findByBranchScoped(UUID tenantId, UUID branchId, Pageable pageable);


    @Query("""
    SELECT i FROM InventoryItem i
    WHERE i.tenantId = :tenantId
      AND i.productId = :productId
      AND i.deleted = false
""")
    List<InventoryItem> findByProductScoped(UUID tenantId, UUID productId);

    /* =====================================================
       BULK SAFE OPS
    ===================================================== */

    @Transactional
    @Modifying
    @Query("""
        update InventoryItem i
        set i.deleted = true
        where i.productVariantId in :variantIds
          AND i.tenantId = :tenantId
    """)
    void softDeleteByVariantIds(
            @Param("variantIds") List<UUID> variantIds,
            @Param("tenantId") UUID tenantId
    );

    @Transactional
    @Modifying
    @Query("""
        update InventoryItem i
        set i.deleted = false
        where i.productVariantId in :variantIds
          AND i.tenantId = :tenantId
          AND i.branchId = :branchId
    """)
    void restoreByVariantIds(
            @Param("variantIds") List<UUID> variantIds,
            @Param("tenantId") UUID tenantId,
            @Param("branchId") UUID branchId
    );

    /* =====================================================
       DEAD STOCK (SCOPED)
    ===================================================== */

    @Query("""
        SELECT i FROM InventoryItem i
        WHERE i.tenantId = :tenantId
          AND i.branchId = :branchId
          AND i.quantityOnHand > 0
          AND i.deleted = false
          AND NOT EXISTS (
              SELECT t FROM StockTransaction t
              WHERE t.productVariantId = i.productVariantId
                AND t.branchId = :branchId
                AND t.type = 'SALE'
                AND t.timestamp >= :cutoff
          )
    """)
    List<InventoryItem> findDeadStock(
            @Param("tenantId") UUID tenantId,
            @Param("branchId") UUID branchId,
            @Param("cutoff") LocalDateTime cutoff
    );

    @Query("""
        SELECT i FROM InventoryItem i
        WHERE i.tenantId = :tenantId
          AND i.deleted = false
          AND i.quantityOnHand <= :threshold
    """)
    Page<InventoryItem> findLowStock(
            UUID tenantId,
            long threshold,
            Pageable pageable
    );

    @Query("""
        SELECT i FROM InventoryItem i
        WHERE i.tenantId = :tenantId
          AND i.deleted = false
          AND i.quantityOnHand <= 0
    """)
    Page<InventoryItem> findOutOfStock(
            UUID tenantId,
            Pageable pageable
    );

    @Query("""
        SELECT i FROM InventoryItem i
        WHERE i.productVariantId IN :variantIds
          AND i.tenantId = :tenantId
          AND i.branchId = :branchId
          AND i.deleted = false
    """)
    List<InventoryItem> findAllByVariants(
            @Param("variantIds") List<UUID> variantIds,
            @Param("tenantId") UUID tenantId,
            @Param("branchId") UUID branchId
    );
}