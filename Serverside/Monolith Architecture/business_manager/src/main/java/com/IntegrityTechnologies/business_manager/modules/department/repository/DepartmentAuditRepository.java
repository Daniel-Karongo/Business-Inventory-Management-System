package com.IntegrityTechnologies.business_manager.modules.department.repository;

import com.IntegrityTechnologies.business_manager.modules.department.model.DepartmentAudit;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

@Repository
public interface DepartmentAuditRepository extends JpaRepository<DepartmentAudit, UUID> {
    List<DepartmentAudit> findByDepartmentIdOrderByTimestampDesc(UUID departmentId);
    List<DepartmentAudit> findByPerformedByIdOrderByTimestampDesc(UUID performedById);
    List<DepartmentAudit> findAllByOrderByTimestampDesc();
}