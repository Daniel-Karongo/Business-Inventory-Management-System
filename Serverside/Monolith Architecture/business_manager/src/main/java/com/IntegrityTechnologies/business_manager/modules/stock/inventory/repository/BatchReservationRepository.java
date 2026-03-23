package com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository;

import com.IntegrityTechnologies.business_manager.modules.stock.inventory.model.BatchReservation;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository.projection.BatchReservationSummary;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository.projection.VariantReservationSummary;
import jakarta.persistence.LockModeType;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Lock;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Map;
import java.util.UUID;

@Repository
public interface BatchReservationRepository
        extends JpaRepository<BatchReservation, UUID> {

    List<BatchReservation> findByReferenceIdAndTenantIdAndBranchId(
            UUID referenceId,
            UUID tenantId,
            UUID branchId
    );

    @Modifying
    @Query("""
        DELETE FROM BatchReservation r
        WHERE r.referenceId = :referenceId
          AND r.tenantId = :tenantId
          AND r.branchId = :branchId
    """)
    void deleteByReferenceId(
            UUID referenceId,
            UUID tenantId,
            UUID branchId
    );

    @Query("""
        SELECT COALESCE(SUM(r.quantity), 0)
        FROM BatchReservation r
        WHERE r.productVariantId = :variantId
          AND r.tenantId = :tenantId
          AND r.branchId = :branchId
    """)
    long sumReservedByVariantAndTenantAndBranch(
            UUID variantId,
            UUID tenantId,
            UUID branchId
    );

    @Lock(LockModeType.PESSIMISTIC_WRITE)
    @Query("""
        SELECT r FROM BatchReservation r
        WHERE r.referenceId = :referenceId
          AND r.tenantId = :tenantId
          AND r.branchId = :branchId
    """)
    List<BatchReservation> lockByReferenceIdAndTenantIdAndBranchId(
            UUID referenceId,
            UUID tenantId,
            UUID branchId
    );

    boolean existsByReferenceIdAndProductVariantIdAndTenantIdAndBranchId(
            UUID referenceId,
            UUID productVariantId,
            UUID tenantId,
            UUID branchId
    );

    @Query("""
        SELECT 
            r.productVariantId AS productVariantId,
            r.batch.branchId AS branchId,
            SUM(r.quantity) AS totalReserved
        FROM BatchReservation r
        WHERE r.tenantId = :tenantId
          AND r.productVariantId IN :variantIds
        GROUP BY r.productVariantId, r.batch.branchId
    """)
    List<VariantReservationSummary> sumReservedBulk(
            @Param("variantIds") List<UUID> variantIds,
            @Param("tenantId") UUID tenantId
    );

    @Query("""
        SELECT r.batch.id AS batchId, SUM(r.quantity) AS totalReserved
        FROM BatchReservation r
        WHERE r.tenantId = :tenantId
          AND r.branchId = :branchId
          AND r.productVariantId = :variantId
        GROUP BY r.batch.id
    """)
    List<BatchReservationSummary> sumReservedByBatch(
            @Param("variantId") UUID variantId,
            @Param("tenantId") UUID tenantId,
            @Param("branchId") UUID branchId
    );

    boolean existsByProductVariantIdAndTenantIdAndBranchId(
            UUID productVariantId,
            UUID tenantId,
            UUID branchId
    );
}