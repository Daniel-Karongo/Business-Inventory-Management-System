package com.IntegrityTechnologies.business_manager.modules.person.entity.user.repository;

import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.UserAudit;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.UUID;

public interface UserAuditRepository extends JpaRepository<UserAudit, UUID> {
    List<UserAudit> findByUserIdOrderByTimestampDesc(UUID userId);
    List<UserAudit> findByPerformedByIdOrderByTimestampDesc(UUID performedById);
}