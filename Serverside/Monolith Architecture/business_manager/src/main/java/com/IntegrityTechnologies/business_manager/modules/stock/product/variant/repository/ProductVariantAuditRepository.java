package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.repository;

import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.model.ProductVariantAudit;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.UUID;

public interface ProductVariantAuditRepository extends JpaRepository<ProductVariantAudit, UUID> {

    List<ProductVariantAudit> findByTenantIdAndBranchIdAndProductVariantIdOrderByTimestampDesc(
            UUID tenantId,
            UUID branchId,
            UUID variantId
    );

    List<ProductVariantAudit> findTop10ByTenantIdAndBranchIdOrderByTimestampDesc(
            UUID tenantId,
            UUID branchId
    );
}