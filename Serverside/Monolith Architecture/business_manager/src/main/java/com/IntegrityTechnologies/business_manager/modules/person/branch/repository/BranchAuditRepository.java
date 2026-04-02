package com.IntegrityTechnologies.business_manager.modules.person.branch.repository;

import com.IntegrityTechnologies.business_manager.modules.person.branch.model.BranchAudit;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

@Repository
public interface BranchAuditRepository extends JpaRepository<BranchAudit, UUID> {

    List<BranchAudit> findByTenantIdAndBranchIdOrderByTimestampDesc(
            UUID tenantId,
            UUID branchId
    );

    List<BranchAudit> findTop10ByTenantIdAndBranchIdOrderByTimestampDesc(
            UUID tenantId,
            UUID branchId
    );

    List<BranchAudit> findByTenantIdAndBranchIdAndPerformedByIdOrderByTimestampDesc(
            UUID tenantId,
            UUID branchId,
            UUID performedById
    );
}