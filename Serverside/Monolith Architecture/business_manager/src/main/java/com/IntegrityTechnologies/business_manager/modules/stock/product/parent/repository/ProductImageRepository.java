package com.IntegrityTechnologies.business_manager.modules.stock.product.parent.repository;

import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.dto.ProductImageProjection;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.model.ProductImage;
import org.springframework.data.jpa.repository.*;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.*;

@Repository
public interface ProductImageRepository extends JpaRepository<ProductImage, UUID> {

    List<ProductImage> findByTenantIdAndBranchIdAndProduct_Id(UUID tenantId, UUID branchId, UUID productId);

    @Modifying
    @Query("""
        update ProductImage i
        set i.deleted = true
        where i.product.id = :productId
          and i.tenantId = :tenantId
          and i.branchId = :branchId
    """)
    void softDeleteByProductId(
            @Param("productId") UUID productId,
            @Param("tenantId") UUID tenantId,
            @Param("branchId") UUID branchId
    );

    @Modifying
    @Query("""
        update ProductImage i
        set i.deleted = false
        where i.product.id = :productId
          and i.deletedIndependently = false
          and i.tenantId = :tenantId
          and i.branchId = :branchId
    """)
    void restoreByProductId(
            @Param("productId") UUID productId,
            @Param("tenantId") UUID tenantId,
            @Param("branchId") UUID branchId
    );

    @Modifying
    @Query("""
        delete from ProductImage i
        where i.product.id = :productId
          and i.tenantId = :tenantId
          and i.branchId = :branchId
    """)
    void deleteByProductId(
            @Param("productId") UUID productId,
            @Param("tenantId") UUID tenantId,
            @Param("branchId") UUID branchId
    );

    Optional<ProductImage> findFirstByTenantIdAndBranchIdAndContentHash(
            UUID tenantId,
            UUID branchId,
            String contentHash
    );

    boolean existsByTenantIdAndBranchIdAndContentHashAndProduct_IdNot(
            UUID tenantId,
            UUID branchId,
            String contentHash,
            UUID productId
    );

    @Query("""
        SELECT p.id as productId, i.filePath as filePath
        FROM Product p
        LEFT JOIN p.images i
        WHERE p.tenantId = :tenantId
        AND p.branchId = :branchId
    """)
    List<ProductImageProjection> findAllImagePaths(UUID tenantId, UUID branchId);
}