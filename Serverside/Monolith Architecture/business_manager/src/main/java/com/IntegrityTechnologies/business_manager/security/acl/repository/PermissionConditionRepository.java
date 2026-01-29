package com.IntegrityTechnologies.business_manager.security.acl.repository;

import com.IntegrityTechnologies.business_manager.security.acl.entity.PermissionCondition;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.UUID;

public interface PermissionConditionRepository extends JpaRepository<PermissionCondition, UUID> {

    List<PermissionCondition> findByPermission_CodeIgnoreCaseAndActiveTrue(String code);

    void deleteByPermission_Id(UUID id);
}