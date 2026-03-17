package com.IntegrityTechnologies.business_manager.modules.stock.category.repository;

import com.IntegrityTechnologies.business_manager.modules.stock.category.model.Category;
import org.springframework.data.jpa.repository.*;
import org.springframework.data.repository.query.Param;

import java.util.*;

public interface CategoryRepository extends JpaRepository<Category, Long> {

    /* =====================================================
       🔴 STRICT LOOKUPS (NO FILTER DEPENDENCE)
    ===================================================== */

    @Query("""
        SELECT c FROM Category c
        WHERE c.id = :id
          AND c.tenantId = :tenantId
          AND c.branchId = :branchId
          AND (:deleted IS NULL OR c.deleted = :deleted)
    """)
    Optional<Category> findByIdSafe(
            Long id,
            Boolean deleted,
            UUID tenantId,
            UUID branchId
    );

    @Query("""
        SELECT c FROM Category c
        WHERE LOWER(c.name) = LOWER(:name)
          AND c.tenantId = :tenantId
          AND c.branchId = :branchId
          AND (:deleted IS NULL OR c.deleted = :deleted)
    """)
    Optional<Category> findByNameSafe(
            String name,
            Boolean deleted,
            UUID tenantId,
            UUID branchId
    );

    @Query("""
        SELECT COUNT(c) > 0 FROM Category c
        WHERE LOWER(c.name) = LOWER(:name)
          AND c.tenantId = :tenantId
          AND c.branchId = :branchId
    """)
    boolean existsByNameSafe(
            String name,
            UUID tenantId,
            UUID branchId
    );

    @Query("""
        SELECT COUNT(c) > 0 FROM Category c
        WHERE LOWER(c.name) = LOWER(:name)
          AND c.parent.id = :parentId
          AND c.tenantId = :tenantId
          AND c.branchId = :branchId
    """)
    boolean existsByNameAndParentSafe(
            String name,
            Long parentId,
            UUID tenantId,
            UUID branchId
    );

    /* =====================================================
       🔴 BULK FETCH (SAFE)
    ===================================================== */

    @Query("""
        SELECT c FROM Category c
        WHERE c.id IN :ids
          AND c.tenantId = :tenantId
          AND c.branchId = :branchId
    """)
    List<Category> findAllByIdSafe(
            @Param("ids") Collection<Long> ids,
            @Param("tenantId") UUID tenantId,
            @Param("branchId") UUID branchId
    );

    /* =====================================================
       🔴 SEARCH (SAFE)
    ===================================================== */

    @Query("""
        SELECT c FROM Category c
        WHERE c.tenantId = :tenantId
          AND c.branchId = :branchId
          AND (:deleted IS NULL OR c.deleted = :deleted)
          AND (
              LOWER(c.name) LIKE LOWER(CONCAT('%', :keyword, '%'))
              OR LOWER(c.description) LIKE LOWER(CONCAT('%', :keyword, '%'))
          )
    """)
    List<Category> searchSafe(
            String keyword,
            Boolean deleted,
            UUID tenantId,
            UUID branchId
    );

    /* =====================================================
       🔴 TREE OPERATIONS (SAFE)
    ===================================================== */

    @Query("""
        SELECT c FROM Category c
        WHERE c.path LIKE CONCAT(:path, '%')
          AND c.tenantId = :tenantId
          AND c.branchId = :branchId
    """)
    List<Category> findSubtreeSafe(
            String path,
            UUID tenantId,
            UUID branchId
    );

    @Modifying
    @Query("""
        UPDATE Category c
        SET c.deleted = true,
            c.deletedAt = CURRENT_TIMESTAMP
        WHERE c.path LIKE CONCAT(:path, '%')
          AND c.tenantId = :tenantId
          AND c.branchId = :branchId
    """)
    void softDeleteByPathSafe(
            String path,
            UUID tenantId,
            UUID branchId
    );

    @Modifying
    @Query("""
        UPDATE Category c
        SET c.deleted = false,
            c.deletedAt = NULL
        WHERE c.path LIKE CONCAT(:path, '%')
          AND c.tenantId = :tenantId
          AND c.branchId = :branchId
    """)
    void restoreByPathSafe(
            String path,
            UUID tenantId,
            UUID branchId
    );

    @Modifying
    @Query("""
        DELETE FROM Category c
        WHERE c.path LIKE CONCAT(:path, '%')
          AND c.tenantId = :tenantId
          AND c.branchId = :branchId
    """)
    void hardDeleteByPathSafe(
            String path,
            UUID tenantId,
            UUID branchId
    );

    /* =====================================================
       🔴 ORDERED FETCH
    ===================================================== */

    @Query("""
        SELECT c FROM Category c
        WHERE c.tenantId = :tenantId
          AND c.branchId = :branchId
          AND (:deleted IS NULL OR c.deleted = :deleted)
        ORDER BY c.path
    """)
    List<Category> findAllOrderedSafe(
            Boolean deleted,
            UUID tenantId,
            UUID branchId
    );

    /* =====================================================
       🔴 RELATION CLEANUP (SAFE)
    ===================================================== */

    @Modifying
    @Query(value = """
        DELETE cs FROM category_suppliers cs
        JOIN categories c ON c.id = cs.category_id
        WHERE cs.category_id = :categoryId
          AND c.tenant_id = :tenantId
          AND c.branch_id = :branchId
    """, nativeQuery = true)
    void detachSuppliersSafe(
            Long categoryId,
            UUID tenantId,
            UUID branchId
    );

    @Modifying
    @Query(value = """
        UPDATE products p
        JOIN categories c ON p.category_id = c.id
        SET p.category_id = NULL
        WHERE c.id = :categoryId
          AND c.tenant_id = :tenantId
          AND c.branch_id = :branchId
    """, nativeQuery = true)
    void detachProductsSafe(
            Long categoryId,
            UUID tenantId,
            UUID branchId
    );

    /* =====================================================
   🔴 PATH REWRITE (SAFE)
===================================================== */

    @Modifying
    @Query("""
    UPDATE Category c
    SET c.path = CONCAT(:newPrefix, SUBSTRING(c.path, LENGTH(:oldPrefix) + 1))
    WHERE c.path LIKE CONCAT(:oldPrefix, '%')
      AND c.tenantId = :tenantId
      AND c.branchId = :branchId
""")
    void rewriteSubtreePathSafe(
            String oldPrefix,
            String newPrefix,
            UUID tenantId,
            UUID branchId
    );

/* =====================================================
   🔴 SUBCATEGORY IDS (SAFE)
===================================================== */

    @Query(value = """
    SELECT c.id FROM categories c
    WHERE c.parent_id = :categoryId
      AND c.tenant_id = :tenantId
      AND c.branch_id = :branchId
""", nativeQuery = true)
    List<Long> findSubcategoryIdsSafe(
            Long categoryId,
            UUID tenantId,
            UUID branchId
    );
}