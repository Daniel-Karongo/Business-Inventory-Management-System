package com.IntegrityTechnologies.business_manager.modules.person.supplier.repository;

import com.IntegrityTechnologies.business_manager.modules.person.supplier.model.Supplier;
import org.springframework.data.domain.*;
import org.springframework.data.jpa.repository.*;
import org.springframework.data.repository.query.Param;

import java.util.*;

public interface SupplierRepository extends JpaRepository<Supplier, UUID>, JpaSpecificationExecutor<Supplier> {

    /* =====================================================
       BASE SAFE FETCH (STRICT BRANCH)
    ===================================================== */
    @Query("""
        SELECT s FROM Supplier s
        WHERE s.id = :id
          AND s.tenantId = :tenantId
          AND s.branchId = :branchId
          AND (:deleted IS NULL OR s.deleted = :deleted)
    """)
    Optional<Supplier> findByIdSafe(
            UUID id,
            Boolean deleted,
            UUID tenantId,
            UUID branchId
    );



    @Query("""
        SELECT s FROM Supplier s
        WHERE s.tenantId = :tenantId
          AND s.branchId = :branchId
          AND s.deleted = false
    """)
    Page<Supplier> findActive(
            @Param("tenantId") UUID tenantId,
            @Param("branchId") UUID branchId,
            Pageable pageable
    );

    @Query("""
        SELECT s FROM Supplier s
        WHERE s.tenantId = :tenantId
          AND s.branchId = :branchId
          AND s.deleted = true
    """)
    Page<Supplier> findDeleted(
            UUID tenantId,
            UUID branchId,
            Pageable pageable
    );

    /* =====================================================
       ADMIN (ALL BRANCHES)
    ===================================================== */

    @Query("""
        SELECT s FROM Supplier s
        WHERE s.tenantId = :tenantId
          AND s.deleted = false
    """)
    Page<Supplier> findAllBranches(
            UUID tenantId,
            Pageable pageable
    );

    /* =====================================================
       SINGLE FETCH
    ===================================================== */

    @Query("""
        SELECT s FROM Supplier s
        WHERE s.id = :id
          AND s.tenantId = :tenantId
          AND s.branchId = :branchId
          AND s.deleted = false
    """)
    Optional<Supplier> findActiveById(
            UUID id,
            UUID tenantId,
            UUID branchId
    );

    /* =====================================================
       EXISTENCE (STRICT)
    ===================================================== */

    @Query("""
        SELECT s FROM Supplier s
        WHERE s.tenantId = :tenantId
          AND s.branchId = :branchId
          AND (:deleted IS NULL OR s.deleted = :deleted)
    """)
    List<Supplier> findAllSafe(
            Boolean deleted,
            UUID tenantId,
            UUID branchId
    );

    @Query("""
        SELECT COUNT(s) > 0 FROM Supplier s
        WHERE lower(s.name) = lower(:name)
          AND s.tenantId = :tenantId
          AND s.branchId = :branchId
    """)
    boolean existsByNameSafe(
            String name,
            UUID tenantId,
            UUID branchId
    );

    @Query("""
        SELECT s FROM Supplier s
        JOIN s.email e
        WHERE lower(e) = lower(:email)
          AND s.tenantId = :tenantId
          AND s.branchId = :branchId
          AND (:deleted IS NULL OR s.deleted = :deleted)
    """)
    Optional<Supplier> findByEmailSafe(
            String email,
            Boolean deleted,
            UUID tenantId,
            UUID branchId
    );

    @Query("""
        SELECT s FROM Supplier s
        JOIN s.phoneNumber p
        WHERE p = :phone
          AND s.tenantId = :tenantId
          AND s.branchId = :branchId
          AND (:deleted IS NULL OR s.deleted = :deleted)
    """)
    Optional<Supplier> findByPhoneSafe(
            String phone,
            Boolean deleted,
            UUID tenantId,
            UUID branchId
    );

    @Query("""
        SELECT s FROM Supplier s
        WHERE lower(s.name) = lower(:name)
          AND s.tenantId = :tenantId
          AND s.branchId = :branchId
          AND (:deleted IS NULL OR s.deleted = :deleted)
    """)
    Optional<Supplier> findByNameSafe(
            String name,
            Boolean deleted,
            UUID tenantId,
            UUID branchId
    );

    @Query("""
        SELECT s FROM Supplier s
        WHERE s.id IN :ids
          AND s.tenantId = :tenantId
          AND s.branchId = :branchId
    """)
    List<Supplier> findAllByIdsSafe(
            List<UUID> ids,
            UUID tenantId,
            UUID branchId
    );

    /* =====================================================
       BULK OPERATIONS
    ===================================================== */

    @Modifying
    @Query("""
        DELETE FROM Supplier s
        WHERE s.id IN :ids
          AND s.tenantId = :tenantId
          AND s.branchId = :branchId
    """)
    void deleteSuppliersByIds(
            List<UUID> ids,
            UUID tenantId,
            UUID branchId
    );

    /* =====================================================
       CATEGORY DETACH (SAFE)
    ===================================================== */

    @Modifying
    @Query(value = """
        DELETE FROM category_suppliers
        WHERE supplier_id IN :ids
          AND tenant_id = :tenantId
          AND branch_id = :branchId
    """, nativeQuery = true)
    void detachFromCategories(
            List<UUID> ids,
            UUID tenantId,
            UUID branchId
    );

    @Modifying
    @Query(value = """
        DELETE ps FROM products_suppliers ps
        JOIN suppliers s ON s.id = ps.supplier_id
        WHERE ps.supplier_id IN :ids
          AND s.tenant_id = :tenantId
          AND s.branch_id = :branchId
    """, nativeQuery = true)
    void detachFromProducts(
            List<UUID> ids,
            UUID tenantId,
            UUID branchId
    );
}