package com.IntegrityTechnologies.business_manager.modules.user.repository;

import com.IntegrityTechnologies.business_manager.modules.user.model.UserImageAudit;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.UUID;

public interface UserImageAuditRepository extends JpaRepository<UserImageAudit, UUID> {
    List<UserImageAudit> findByUserId(UUID userId);
    List<UserImageAudit> findByPerformedById(UUID performedById);
}