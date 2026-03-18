package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.repository;

import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.model.ProductVariantImage;
import org.springframework.data.jpa.repository.*;
import org.springframework.data.repository.query.Param;

import java.util.*;

public interface ProductVariantImageRepository extends JpaRepository<ProductVariantImage, UUID> {

    List<ProductVariantImage> findByTenantIdAndBranchIdAndVariant_IdIn(
            UUID tenantId,
            UUID branchId,
            List<UUID> variantIds
    );

    List<ProductVariantImage> findByTenantIdAndBranchIdAndVariant_IdAndDeletedFalse(
            UUID tenantId,
            UUID branchId,
            UUID variantId
    );

    Optional<ProductVariantImage> findByTenantIdAndBranchIdAndFileName(
            UUID tenantId,
            UUID branchId,
            String fileName
    );

    @Query("""
    select pvi
    from ProductVariantImage pvi
    join fetch pvi.variant v
    where v.id in :variantIds
      and pvi.tenantId = :tenantId
      and pvi.branchId = :branchId
""")
    List<ProductVariantImage> findByVariantIdsWithVariant(
            @Param("variantIds") List<UUID> variantIds,
            @Param("tenantId") UUID tenantId,
            @Param("branchId") UUID branchId
    );

    @Modifying
    @Query("""
        update ProductVariantImage i
        set i.deleted = true
        where i.variant.id in :variantIds
          and i.tenantId = :tenantId
          and i.branchId = :branchId
    """)
    void softDeleteByVariantIds(
            @Param("variantIds") List<UUID> variantIds,
            @Param("tenantId") UUID tenantId,
            @Param("branchId") UUID branchId
    );

    @Modifying
    @Query("""
        update ProductVariantImage i
        set i.deleted = false
        where i.variant.id in :variantIds
          and i.tenantId = :tenantId
          and i.branchId = :branchId
    """)
    void restoreByVariantIds(
            @Param("variantIds") List<UUID> variantIds,
            @Param("tenantId") UUID tenantId,
            @Param("branchId") UUID branchId
    );

    @Modifying
    @Query("""
        delete from ProductVariantImage i
        where i.variant.id in :variantIds
          and i.tenantId = :tenantId
          and i.branchId = :branchId
    """)
    void deleteByVariantIds(
            @Param("variantIds") List<UUID> variantIds,
            @Param("tenantId") UUID tenantId,
            @Param("branchId") UUID branchId
    );

    Optional<ProductVariantImage> findFirstByTenantIdAndBranchIdAndContentHash(
            UUID tenantId,
            UUID branchId,
            String contentHash
    );

    Optional<ProductVariantImage> findByTenantIdAndBranchIdAndVariant_IdAndFileNameAndDeletedFalse(
            UUID tenantId,
            UUID branchId,
            UUID variantId,
            String fileName
    );
}