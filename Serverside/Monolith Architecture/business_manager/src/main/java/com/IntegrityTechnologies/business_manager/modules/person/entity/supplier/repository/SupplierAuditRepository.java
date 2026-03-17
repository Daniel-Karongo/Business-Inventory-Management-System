package com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.repository;

import com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.model.SupplierAudit;
import org.springframework.data.domain.*;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.*;

public interface SupplierAuditRepository extends JpaRepository<SupplierAudit, UUID> {

    /* ========= EXISTING ========= */

    List<SupplierAudit> findBySupplierIdOrderByTimestampDesc(UUID supplierId);

    List<SupplierAudit> findAllByOrderByTimestampDesc();

    List<SupplierAudit> findTop5ByOrderByTimestampDesc();


    /* ========= SAFE ========= */

    Page<SupplierAudit> findBySupplierIdAndTenantIdAndBranchIdOrderByTimestampDesc(
            UUID supplierId,
            UUID tenantId,
            UUID branchId,
            Pageable pageable
    );

    Page<SupplierAudit> findByTenantIdOrderByTimestampDesc(
            UUID tenantId,
            Pageable pageable
    );

    List<SupplierAudit> findTop5ByTenantIdAndBranchIdOrderByTimestampDesc(
            UUID tenantId,
            UUID branchId
    );

    List<SupplierAudit> findBySupplierIdAndTenantIdAndBranchIdOrderByTimestampDesc(
            UUID supplierId,
            UUID tenantId,
            UUID branchId
    );

    List<SupplierAudit> findAllByTenantIdAndBranchIdOrderByTimestampDesc(
            UUID tenantId,
            UUID branchId
    );
}