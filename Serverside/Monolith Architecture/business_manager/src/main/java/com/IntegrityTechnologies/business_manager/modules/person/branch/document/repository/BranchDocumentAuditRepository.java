package com.IntegrityTechnologies.business_manager.modules.person.branch.document.repository;

import com.IntegrityTechnologies.business_manager.modules.person.branch.document.model.BranchDocumentAudit;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.UUID;

public interface BranchDocumentAuditRepository
        extends JpaRepository<BranchDocumentAudit,UUID> {

    List<BranchDocumentAudit>
    findByTenantIdAndBranchIdOrderByTimestampDesc(
            UUID tenantId,
            UUID branchId
    );

    List<BranchDocumentAudit>
    findByTenantIdAndPerformedByIdOrderByTimestampDesc(
            UUID tenantId,
            UUID performedById
    );
}