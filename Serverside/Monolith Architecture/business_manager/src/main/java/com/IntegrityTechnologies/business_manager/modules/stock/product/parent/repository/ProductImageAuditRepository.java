package com.IntegrityTechnologies.business_manager.modules.stock.product.parent.repository;

import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.model.ProductImageAudit;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.*;

public interface ProductImageAuditRepository extends JpaRepository<ProductImageAudit, UUID> {

    List<ProductImageAudit> findByTenantIdAndBranchIdAndProductId(
            UUID tenantId,
            UUID branchId,
            UUID productId
    );

    List<ProductImageAudit> findByTenantIdAndBranchId(UUID tenantId, UUID branchId);
}