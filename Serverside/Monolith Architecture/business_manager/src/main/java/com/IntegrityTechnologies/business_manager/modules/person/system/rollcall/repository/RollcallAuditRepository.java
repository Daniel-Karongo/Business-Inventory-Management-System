package com.IntegrityTechnologies.business_manager.modules.person.system.rollcall.repository;

import com.IntegrityTechnologies.business_manager.modules.person.system.rollcall.model.RollcallAudit;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

@Repository
public interface RollcallAuditRepository extends JpaRepository<RollcallAudit, UUID> {
    List<RollcallAudit> findByBranchIdOrderByTimestampDesc(UUID branchId);
    List<RollcallAudit> findByUserIdOrderByTimestampDesc(UUID userId);
}
