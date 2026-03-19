package com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository;

import com.IntegrityTechnologies.business_manager.modules.stock.inventory.model.InventoryBatch;
import jakarta.persistence.LockModeType;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Lock;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

@Repository
public interface InventoryBatchRepository
        extends JpaRepository<InventoryBatch, UUID> {

    /* =====================================================
       FIFO LOCKED BATCH FETCH
    ===================================================== */

    @Lock(LockModeType.PESSIMISTIC_WRITE)
    @Query("""
        SELECT b FROM InventoryBatch b
        WHERE b.productVariantId = :variantId
          AND b.tenantId = :tenantId
          AND b.branchId = :branchId
          AND b.quantityRemaining > 0
        ORDER BY b.receivedAt ASC
    """)
    List<InventoryBatch> lockAvailableBatchesFIFO(
            @Param("variantId") UUID variantId,
            @Param("tenantId") UUID tenantId,
            @Param("branchId") UUID branchId
    );

    @Lock(LockModeType.PESSIMISTIC_WRITE)
    @Query("""
    SELECT b FROM InventoryBatch b
    WHERE b.productVariantId = :variantId
      AND b.branchId = :branchId
      AND b.tenantId = :tenantId
      AND b.quantityRemaining > 0
    ORDER BY b.receivedAt ASC
""")
    List<InventoryBatch> lockBatchesByBranch(
            UUID variantId,
            UUID tenantId,
            UUID branchId
    );

    /* =====================================================
       READ (PAGINATED)
    ===================================================== */

    @Query("""
        SELECT b FROM InventoryBatch b
        WHERE b.tenantId = :tenantId
          AND b.branchId = :branchId
          AND b.productVariantId = :variantId
    """)
    Page<InventoryBatch> findAllByVariant(
            @Param("tenantId") UUID tenantId,
            @Param("branchId") UUID branchId,
            @Param("variantId") UUID variantId,
            Pageable pageable
    );

    @Query("""
        SELECT b FROM InventoryBatch b
        WHERE b.productVariantId = :variantId
          AND b.tenantId = :tenantId
          AND b.branchId = :branchId
          AND b.quantityRemaining > 0
        ORDER BY b.receivedAt ASC
    """)
    List<InventoryBatch> findAvailableBatches(
            @Param("variantId") UUID variantId,
            @Param("tenantId") UUID tenantId,
            @Param("branchId") UUID branchId
    );

    Optional<InventoryBatch> findByIdAndTenantIdAndBranchId(
            UUID id,
            UUID tenantId,
            UUID branchId
    );

    @Lock(LockModeType.PESSIMISTIC_WRITE)
        @Query("""
        SELECT b FROM InventoryBatch b
        WHERE b.id = :batchId
        AND b.tenantId = :tenantId
        AND b.branchId = :branchId
    """)
    Optional<InventoryBatch> findByIdForUpdate(
            UUID batchId,
            UUID tenantId,
            UUID branchId
    );
}