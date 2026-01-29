package com.IntegrityTechnologies.business_manager.security.acl.repository;

import com.IntegrityTechnologies.business_manager.security.acl.entity.RoleEntity;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;
import java.util.UUID;

public interface RoleEntityRepository extends JpaRepository<RoleEntity, UUID> {

    Optional<RoleEntity> findByNameIgnoreCase(String name);
}