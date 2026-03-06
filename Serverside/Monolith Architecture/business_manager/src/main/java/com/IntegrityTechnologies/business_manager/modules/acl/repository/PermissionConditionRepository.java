package com.IntegrityTechnologies.business_manager.modules.acl.repository;

import com.IntegrityTechnologies.business_manager.modules.acl.entity.PermissionCondition;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.Role;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List;
import java.util.UUID;

public interface PermissionConditionRepository
        extends JpaRepository<PermissionCondition, UUID> {
    @Modifying
    @Query("""
        update PermissionCondition pc
        set pc.active = false
        where pc.role.id = :roleId
    """)
    int softDeleteByRoleId(@Param("roleId") UUID roleId);

    @Modifying
    @Query("""
        update PermissionCondition pc
        set pc.active = false
        where pc.permission.id = :permissionId
    """)
    int softDeleteByPermissionId(@Param("permissionId") UUID permissionId);
    List<PermissionCondition>
    findByPermission_IdAndRole_NameAndActiveTrue(UUID permissionId, Role role);
}