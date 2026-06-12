package com.IntegrityTechnologies.business_manager.modules.stock.product.parent.repository;

import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.model.Product;
import jakarta.persistence.LockModeType;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.*;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

@Repository
public interface ProductRepository extends JpaRepository<Product, UUID>, JpaSpecificationExecutor<Product> {

    @Query(
            value = """
                    SELECT
                        p.id as id,
                        p.name as name,
                        p.sku as sku,
                        p.deleted as deleted,
                        p.createdAt as createdAt,
                        p.updatedAt as updatedAt,
                        c.id as categoryId,
                        c.name as categoryName,
                        primaryImage.thumbnailFileName as thumbnailFileName,
                        primaryImage.fileName as primaryImageFileName,
                        COUNT(DISTINCT v.id) as variantCount
                    FROM Product p
                    LEFT JOIN p.category c
                    LEFT JOIN p.variants v
                    LEFT JOIN p.suppliers ps
                    LEFT JOIN p.images primaryImage
                        ON primaryImage.primaryImage = true
                       AND primaryImage.deleted = false
                    WHERE p.tenantId = :tenantId
                      AND p.branchId = :branchId

                      AND (
                            :deleted IS NULL
                            OR p.deleted = :deleted
                      )

                      AND (
                            :categoryId IS NULL
                            OR c.id = :categoryId
                      )

                      AND (
                            :supplierId IS NULL
                            OR ps.supplier.id = :supplierId
                      )

                      AND (
                            :keyword IS NULL
                            OR :keyword = ''
                            OR LOWER(p.name)
                                LIKE LOWER(
                                    CONCAT('%', :keyword, '%')
                                )
                            OR LOWER(
                                    COALESCE(
                                        p.sku,
                                        ''
                                    )
                               )
                                LIKE LOWER(
                                    CONCAT('%', :keyword, '%')
                                )
                      )
                    GROUP BY
                          p.id,
                          p.name,
                          p.sku,
                          p.deleted,
                          p.createdAt,
                          p.updatedAt,
                          c.id,
                          c.name,
                          primaryImage.thumbnailFileName,
                          primaryImage.fileName
                    """,
            countQuery = """
                    SELECT COUNT(DISTINCT p.id)
                    FROM Product p
                    LEFT JOIN p.category c
                    LEFT JOIN p.suppliers ps
                    WHERE p.tenantId = :tenantId
                      AND p.branchId = :branchId

                      AND (
                            :deleted IS NULL
                            OR p.deleted = :deleted
                      )

                      AND (
                            :categoryId IS NULL
                            OR c.id = :categoryId
                      )

                      AND (
                            :supplierId IS NULL
                            OR ps.supplier.id = :supplierId
                      )

                      AND (
                            :keyword IS NULL
                            OR :keyword = ''
                            OR LOWER(p.name)
                                LIKE LOWER(
                                    CONCAT('%', :keyword, '%')
                                )
                            OR LOWER(
                                    COALESCE(
                                        p.sku,
                                        ''
                                    )
                               )
                                LIKE LOWER(
                                    CONCAT('%', :keyword, '%')
                                )
                      )
                    """
    )
    Page<StockWorkspaceProductProjection>
    findWorkspaceProducts(
            UUID tenantId,
            UUID branchId,
            Long categoryId,
            UUID supplierId,
            String keyword,
            Boolean deleted,
            Pageable pageable
    );

    @Override
    @EntityGraph(attributePaths = {
            "category"
    })
    Page<Product> findAll(
            Specification<Product> spec,
            Pageable pageable
    );

    @EntityGraph(attributePaths = {
            "images",
            "variants",
            "suppliers.supplier",
            "category"
    })
    List<Product> findAllWithRelationsByTenantIdAndBranchId(UUID tenantId, UUID branchId);

    boolean existsByTenantIdAndBranchIdAndNameIgnoreCase(UUID tenantId, UUID branchId, String name);

    boolean existsByTenantIdAndBranchIdAndSku(UUID tenantId, UUID branchId, String sku);

    boolean existsByTenantIdAndBranchIdAndSkuAndIdNot(
            UUID tenantId,
            UUID branchId,
            String sku,
            UUID id
    );

    Optional<Product> findByTenantIdAndBranchIdAndSku(UUID tenantId, UUID branchId, String sku);

    Optional<Product> findByTenantIdAndBranchIdAndNameIgnoreCase(UUID tenantId, UUID branchId, String name);

    @EntityGraph(attributePaths = {
            "category",
            "images",
            "variants",
            "suppliers.supplier"
    })
    Optional<Product> findByIdAndTenantIdAndBranchId(
            UUID id,
            UUID tenantId,
            UUID branchId
    );

    @EntityGraph(attributePaths = {
            "category",
            "images",
            "variants",
            "suppliers.supplier"
    })
    Optional<Product> findByIdAndTenantIdAndBranchIdAndDeletedFalse(
            UUID id,
            UUID tenantId,
            UUID branchId
    );

    List<Product> findAllByTenantIdAndBranchIdAndCategory_Id(UUID tenantId, UUID branchId, Long categoryId);

    List<Product> findAllByTenantIdAndBranchIdAndCategory_IdIn(UUID tenantId, UUID branchId, List<Long> categoryIds);

    @Query("""
                SELECT p FROM Product p
                JOIN p.suppliers ps
                WHERE ps.supplier.id = :supplierId
                  AND p.tenantId = :tenantId
                  AND p.branchId = :branchId
            """)
    List<Product> findAllBySupplierIdAndTenantIdAndBranchId(
            UUID supplierId,
            UUID tenantId,
            UUID branchId
    );

    @Lock(LockModeType.PESSIMISTIC_WRITE)
    @Query("""
                select p
                from Product p
                where p.id = :id
                  and p.tenantId = :tenantId
                  and p.branchId = :branchId
            """)
    Optional<Product> findForUpdate(
            @Param("id") UUID id,
            @Param("tenantId") UUID tenantId,
            @Param("branchId") UUID branchId
    );

    @Modifying
    @Query(value = """
                DELETE FROM product_suppliers
                WHERE product_id = :productId
            """, nativeQuery = true)
    void detachSuppliers(@Param("productId") UUID productId);

    @Lock(LockModeType.PESSIMISTIC_WRITE)
    @Query("""
                select p
                from Product p
                where lower(p.name) = lower(:name)
                  and p.tenantId = :tenantId
                  and p.branchId = :branchId
            """)
    Optional<Product> findByNameForUpdate(
            @Param("tenantId") UUID tenantId,
            @Param("branchId") UUID branchId,
            @Param("name") String name
    );
}