package com.IntegrityTechnologies.business_manager.modules.person.supplier.repository;

import com.IntegrityTechnologies.business_manager.modules.person.supplier.model.SupplierImage;
import org.springframework.data.jpa.repository.*;

import java.util.*;

public interface SupplierImageRepository extends JpaRepository<SupplierImage, UUID> {

    /* =====================================================
       ✅ SAFE METHODS (STRICT TENANT + BRANCH)
    ===================================================== */
    @Query("""
        SELECT i FROM SupplierImage i
        WHERE i.tenantId = :tenantId
          AND i.branchId = :branchId
    """)
    List<SupplierImage> findAllSafe(UUID tenantId, UUID branchId);

    @Query("""
        SELECT si FROM SupplierImage si
        JOIN si.supplier s
        WHERE si.tenantId = :tenantId
          AND si.branchId = :branchId
          AND (:deletedImage IS NULL OR si.deleted = :deletedImage)
          AND (:deletedSupplier IS NULL OR s.deleted = :deletedSupplier)
    """)
    List<SupplierImage> findAllWithSupplierFilter(
            UUID tenantId,
            UUID branchId,
            Boolean deletedSupplier,
            Boolean deletedImage
    );

    @Query("""
        SELECT si FROM SupplierImage si
        WHERE si.supplier.id = :supplierId
          AND si.tenantId = :tenantId
          AND si.branchId = :branchId
    """)
    List<SupplierImage> findSafeBySupplier(
            UUID supplierId,
            UUID tenantId,
            UUID branchId
    );

    @Query("""
        SELECT si FROM SupplierImage si
        WHERE si.supplier.id = :supplierId
          AND si.tenantId = :tenantId
          AND si.branchId = :branchId
          AND si.deleted = false
    """)
    List<SupplierImage> findSafeActiveBySupplier(
            UUID supplierId,
            UUID tenantId,
            UUID branchId
    );

    @Query("""
        SELECT si FROM SupplierImage si
        WHERE si.supplier.id = :supplierId
          AND si.tenantId = :tenantId
          AND si.branchId = :branchId
          AND si.fileName = :fileName
    """)
    Optional<SupplierImage> findSafeByFileName(
            UUID supplierId,
            String fileName,
            UUID tenantId,
            UUID branchId
    );

    @Modifying
        @Query("""
        DELETE FROM SupplierImage si
        WHERE si.supplier.id IN :supplierIds
          AND si.tenantId = :tenantId
          AND si.branchId = :branchId
    """)
    void deleteAllBySupplierIds(
            List<UUID> supplierIds,
            UUID tenantId,
            UUID branchId
    );
}