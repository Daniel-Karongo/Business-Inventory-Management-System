package com.IntegrityTechnologies.business_manager.modules.acl.repository;

import com.IntegrityTechnologies.business_manager.modules.acl.entity.RoleEntity;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.Role;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;
import java.util.UUID;

public interface RoleEntityRepository extends JpaRepository<RoleEntity, UUID> {
    Optional<RoleEntity> findByName(Role role);
}