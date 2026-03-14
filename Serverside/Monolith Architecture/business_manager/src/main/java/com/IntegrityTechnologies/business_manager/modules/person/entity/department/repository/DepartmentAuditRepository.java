package com.IntegrityTechnologies.business_manager.modules.person.entity.department.repository;

import com.IntegrityTechnologies.business_manager.modules.person.entity.department.model.DepartmentAudit;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.UUID;

public interface DepartmentAuditRepository extends JpaRepository<DepartmentAudit, UUID> {

    /* =========================================================
       TENANT AUDITS
    ========================================================= */

    List<DepartmentAudit> findByTenantIdOrderByTimestampDesc(
            UUID tenantId
    );

    /* =========================================================
       BRANCH AUDITS
    ========================================================= */

    List<DepartmentAudit> findByTenantIdAndBranchIdOrderByTimestampDesc(
            UUID tenantId,
            UUID branchId
    );

    /* =========================================================
       DEPARTMENT AUDITS
    ========================================================= */

    List<DepartmentAudit> findByTenantIdAndDepartmentIdOrderByTimestampDesc(
            UUID tenantId,
            UUID departmentId
    );

    List<DepartmentAudit> findByTenantIdAndBranchIdAndDepartmentIdOrderByTimestampDesc(
            UUID tenantId,
            UUID branchId,
            UUID departmentId
    );

    /* =========================================================
       PERFORMER AUDITS
    ========================================================= */

    List<DepartmentAudit> findByTenantIdAndPerformedByIdOrderByTimestampDesc(
            UUID tenantId,
            UUID performerId
    );

    List<DepartmentAudit> findByTenantIdAndBranchIdAndPerformedByIdOrderByTimestampDesc(
            UUID tenantId,
            UUID branchId,
            UUID performerId
    );

    /* =========================================================
       ENTERPRISE FEED
    ========================================================= */

    List<DepartmentAudit> findTop200ByTenantIdOrderByTimestampDesc(
            UUID tenantId
    );
}