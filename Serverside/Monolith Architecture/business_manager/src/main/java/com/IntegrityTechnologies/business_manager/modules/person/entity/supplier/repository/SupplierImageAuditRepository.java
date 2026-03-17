package com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.repository;

import com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.model.SupplierImageAudit;
import org.springframework.data.domain.*;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.*;

public interface SupplierImageAuditRepository extends JpaRepository<SupplierImageAudit, UUID> {

    /* ========= EXISTING ========= */

    List<SupplierImageAudit> findBySupplierIdOrderByTimestampDesc(UUID supplierId);

    List<SupplierImageAudit> findAllByOrderByTimestampDesc();


    /* ========= SAFE ========= */

    Page<SupplierImageAudit> findBySupplierIdAndTenantIdAndBranchIdOrderByTimestampDesc(
            UUID supplierId,
            UUID tenantId,
            UUID branchId,
            Pageable pageable
    );

    Page<SupplierImageAudit> findByTenantIdOrderByTimestampDesc(
            UUID tenantId,
            Pageable pageable
    );

    List<SupplierImageAudit> findBySupplierIdAndTenantIdAndBranchIdOrderByTimestampDesc(
            UUID supplierId,
            UUID tenantId,
            UUID branchId
    );

    List<SupplierImageAudit> findAllByTenantIdAndBranchIdOrderByTimestampDesc(
            UUID tenantId,
            UUID branchId
    );
}