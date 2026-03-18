package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.repository;

import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.model.ProductVariantImageAudit;
import org.springframework.data.jpa.repository.*;
import org.springframework.data.repository.query.Param;

import java.util.List;
import java.util.UUID;

public interface ProductVariantImageAuditRepository
        extends JpaRepository<ProductVariantImageAudit, UUID> {

    List<ProductVariantImageAudit> findByTenantIdAndBranchIdAndProductVariantIdIn(
            UUID tenantId,
            UUID branchId,
            List<UUID> variantIds
    );

    @Modifying
    @Query("""
        delete from ProductVariantImageAudit a
        where a.productVariantId in :variantIds
          and a.tenantId = :tenantId
          and a.branchId = :branchId
    """)
    void deleteByVariantIds(
            @Param("variantIds") List<UUID> variantIds,
            @Param("tenantId") UUID tenantId,
            @Param("branchId") UUID branchId
    );
}