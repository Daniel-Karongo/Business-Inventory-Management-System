package com.IntegrityTechnologies.business_manager.modules.rollcall.repository;

import com.IntegrityTechnologies.business_manager.modules.rollcall.model.RollcallAudit;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

@Repository
public interface RollcallAuditRepository extends JpaRepository<RollcallAudit, UUID> {
    List<RollcallAudit> findByDepartmentIdOrderByTimestampDesc(UUID departmentId);
    List<RollcallAudit> findByUserIdOrderByTimestampDesc(UUID userId);
}
