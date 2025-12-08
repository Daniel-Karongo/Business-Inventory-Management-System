package com.IntegrityTechnologies.business_manager.modules.person.entity.branch.repository;

import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.model.BranchAudit;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

@Repository
public interface BranchAuditRepository extends JpaRepository<BranchAudit, UUID> {
    List<BranchAudit> findByBranchIdOrderByTimestampDesc(UUID branchID);
    List<BranchAudit> findByPerformedByIdOrderByTimestampDesc(UUID performedById);
    List<BranchAudit> findAllByOrderByTimestampDesc();
}