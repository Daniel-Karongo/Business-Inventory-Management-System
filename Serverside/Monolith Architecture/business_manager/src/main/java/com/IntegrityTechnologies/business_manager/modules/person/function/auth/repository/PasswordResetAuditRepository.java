package com.IntegrityTechnologies.business_manager.modules.person.function.auth.repository;

import com.IntegrityTechnologies.business_manager.modules.person.function.auth.model.PasswordResetAudit;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.UUID;

public interface PasswordResetAuditRepository extends JpaRepository<PasswordResetAudit, UUID> {}