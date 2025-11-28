package com.IntegrityTechnologies.business_manager.modules.person.function.branch.repository;

import com.IntegrityTechnologies.business_manager.modules.person.function.branch.model.BranchAudit;
import com.IntegrityTechnologies.business_manager.modules.person.function.department.model.DepartmentAudit;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

@Repository
public interface BranchAuditRepository extends JpaRepository<BranchAudit, UUID> {
    List<DepartmentAudit> findByBranchIdOrderByTimestampDesc(UUID branchID);
    List<DepartmentAudit> findByPerformedByIdOrderByTimestampDesc(UUID performedById);
    List<DepartmentAudit> findAllByOrderByTimestampDesc();
}