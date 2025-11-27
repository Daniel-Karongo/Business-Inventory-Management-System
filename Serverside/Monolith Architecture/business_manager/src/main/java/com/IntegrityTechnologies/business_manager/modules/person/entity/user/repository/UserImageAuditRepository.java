package com.IntegrityTechnologies.business_manager.modules.person.entity.user.repository;

import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.UserImageAudit;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.UUID;

public interface UserImageAuditRepository extends JpaRepository<UserImageAudit, UUID> {
    List<UserImageAudit> findByUserIdOrderByTimestampDesc(UUID userId);
    List<UserImageAudit> findByPerformedByIdOrderByTimestampDesc(UUID performedById);
}