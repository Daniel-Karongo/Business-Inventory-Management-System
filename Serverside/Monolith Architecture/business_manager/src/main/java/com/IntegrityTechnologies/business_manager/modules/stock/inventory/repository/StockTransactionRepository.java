package com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository;

import com.IntegrityTechnologies.business_manager.modules.stock.inventory.model.StockTransaction;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

@Repository
public interface StockTransactionRepository extends JpaRepository<StockTransaction, UUID> {

    List<StockTransaction> findByProductIdAndTenantIdOrderByTimestampDesc(
            UUID productId, UUID tenantId
    );

    List<StockTransaction> findByProductVariantIdAndTenantIdOrderByTimestampDesc(
            UUID variantId, UUID tenantId
    );

    List<StockTransaction> findByBranchIdAndTenantIdOrderByTimestampDesc(
            UUID branchId, UUID tenantId
    );

    List<StockTransaction> findByProductIdAndBranchIdAndTenantIdOrderByTimestampDesc(
            UUID productId, UUID branchId, UUID tenantId
    );

    List<StockTransaction> findByProductVariantIdAndBranchIdAndTenantIdOrderByTimestampDesc(
            UUID variantId, UUID branchId, UUID tenantId
    );

    @Query("""
        SELECT s FROM StockTransaction s
        WHERE s.tenantId = :tenantId
          AND s.timestamp BETWEEN :from AND :to
          AND s.deleted = false
        ORDER BY s.timestamp DESC
    """)
    List<StockTransaction> findByDateRange(
            UUID tenantId,
            LocalDateTime from,
            LocalDateTime to
    );

    /* =====================================================
       IDENTITY / IDEMPOTENCY
    ===================================================== */

    boolean existsByReferenceAndTypeAndTenantIdAndBranchId(
            String reference,
            StockTransaction.TransactionType type,
            UUID tenantId,
            UUID branchId
    );

    boolean existsByProductIdAndTenantIdAndBranchId(
            UUID productId,
            UUID tenantId,
            UUID branchId
    );

    /* =====================================================
       PAGINATED READS (STRICT)
    ===================================================== */
    @Query("""
        SELECT s FROM StockTransaction s
        WHERE s.tenantId = :tenantId
          AND s.branchId = :branchId
          AND s.deleted = false
        ORDER BY s.timestamp DESC
    """)
    Page<StockTransaction> findAllScoped(
            UUID tenantId,
            UUID branchId,
            Pageable pageable
    );

    @Query("""
        SELECT s FROM StockTransaction s
        WHERE s.tenantId = :tenantId
          AND s.branchId = :branchId
          AND s.productVariantId = :variantId
          AND s.deleted = false
        ORDER BY s.timestamp DESC
    """)
    Page<StockTransaction> findByVariantScoped(
            UUID tenantId,
            UUID branchId,
            UUID variantId,
            Pageable pageable
    );

    @Query("""
        SELECT s FROM StockTransaction s
        WHERE s.productId = :productId
          AND s.tenantId = :tenantId
          AND s.branchId = :branchId
          AND s.deleted = false
    """)
    List<StockTransaction> findByProductScoped(
            UUID productId,
            UUID tenantId,
            UUID branchId
    );

    /* =====================================================
       FIFO / SNAPSHOT SUPPORT
    ===================================================== */

    @Query("""
    SELECT s FROM StockTransaction s
    WHERE s.productVariantId = :variantId
      AND s.branchId = :branchId
      AND s.tenantId = :tenantId
      AND s.type = 'RECEIPT'
      AND s.deleted = false
""")
    List<StockTransaction> findReceipts(UUID variantId, UUID branchId, UUID tenantId);

    @Query("""
    SELECT s FROM StockTransaction s
    WHERE s.productVariantId = :variantId
      AND s.branchId = :branchId
      AND s.tenantId = :tenantId
      AND s.timestamp BETWEEN :from AND :to
      AND s.deleted = false
    ORDER BY s.timestamp ASC
""")
    List<StockTransaction> findBetweenVariant(
            UUID variantId,
            UUID branchId,
            UUID tenantId,
            LocalDateTime from,
            LocalDateTime to
    );

    /* =====================================================
       BULK SAFE OPS
    ===================================================== */

    @Transactional
    @Modifying
    @Query("""
        update StockTransaction s
        set s.deleted = true
        where s.productVariantId in :variantIds
          AND s.tenantId = :tenantId
    """)
    void softDeleteByVariantIds(
            List<UUID> variantIds,
            UUID tenantId
    );

    @Transactional
    @Modifying
    @Query("""
    update StockTransaction s
    set s.deleted = false
    where s.productVariantId in :variantIds
      AND s.tenantId = :tenantId
      AND s.branchId = :branchId
""")
    void restoreByVariantIds(
            @Param("variantIds") List<UUID> variantIds,
            @Param("tenantId") UUID tenantId,
            @Param("branchId") UUID branchId
    );
}