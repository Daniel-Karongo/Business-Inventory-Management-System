package com.IntegrityTechnologies.business_manager.modules.stock.product.parent.repository;

import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.model.ProductAudit;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.*;

public interface ProductAuditRepository extends JpaRepository<ProductAudit, UUID> {

    List<ProductAudit> findByTenantIdAndBranchIdAndProductIdOrderByTimestampDesc(
            UUID tenantId,
            UUID branchId,
            UUID productId
    );

    List<ProductAudit> findTop5ByTenantIdAndBranchIdOrderByTimestampDesc(
            UUID tenantId,
            UUID branchId
    );
}