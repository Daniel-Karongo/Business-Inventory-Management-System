package com.IntegrityTechnologies.business_manager.modules.stock.product.parent.repository;

import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.model.Product;
import jakarta.persistence.LockModeType;
import org.springframework.data.domain.*;
import org.springframework.data.jpa.repository.*;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.*;

@Repository
public interface ProductRepository extends JpaRepository<Product, UUID>, JpaSpecificationExecutor<Product> {

    @EntityGraph(attributePaths = {
            "images",
            "variants",
            "suppliers.supplier",
            "category"
    })
    List<Product> findAllWithRelationsByTenantIdAndBranchId(UUID tenantId, UUID branchId);

    boolean existsByTenantIdAndBranchIdAndNameIgnoreCase(UUID tenantId, UUID branchId, String name);

    boolean existsByTenantIdAndBranchIdAndSku(UUID tenantId, UUID branchId, String sku);

    Optional<Product> findByTenantIdAndBranchIdAndSku(UUID tenantId, UUID branchId, String sku);

    Optional<Product> findByTenantIdAndBranchIdAndNameIgnoreCase(UUID tenantId, UUID branchId, String name);

    Optional<Product> findByIdAndTenantIdAndBranchId(UUID id, UUID tenantId, UUID branchId);

    Optional<Product> findByIdAndTenantIdAndBranchIdAndDeletedFalse(UUID id, UUID tenantId, UUID branchId);

    List<Product> findAllByTenantIdAndBranchIdAndDeletedFalse(UUID tenantId, UUID branchId);

    List<Product> findAllByTenantIdAndBranchIdAndDeletedTrue(UUID tenantId, UUID branchId);

    Page<Product> findByTenantIdAndBranchIdAndCategory_Id(UUID tenantId, UUID branchId, Long categoryId, Pageable pageable);

    Page<Product> findByTenantIdAndBranchIdAndNameContainingIgnoreCase(UUID tenantId, UUID branchId, String name, Pageable pageable);

    Page<Product> findByTenantIdAndBranchIdAndCategory_IdAndNameContainingIgnoreCase(
            UUID tenantId,
            UUID branchId,
            Long categoryId,
            String name,
            Pageable pageable
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

    List<Product> findAllByIdInAndTenantIdAndBranchId(List<UUID> ids, UUID tenantId, UUID branchId);

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
        DELETE FROM products_suppliers
        WHERE product_id = :productId
    """, nativeQuery = true)
    void detachSuppliers(@Param("productId") UUID productId);
}