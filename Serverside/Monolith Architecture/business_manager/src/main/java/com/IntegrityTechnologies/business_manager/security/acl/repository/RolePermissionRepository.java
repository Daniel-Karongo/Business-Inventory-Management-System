package com.IntegrityTechnologies.business_manager.security.acl.repository;

import com.IntegrityTechnologies.business_manager.security.acl.entity.RolePermission;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface RolePermissionRepository extends JpaRepository<RolePermission, UUID> {

    @Query("""
        select rp 
        from RolePermission rp 
        join fetch rp.permission p 
        where lower(rp.role.name) = lower(:roleName)
          and rp.allowed = true
          and p.active = true
    """)
    List<RolePermission> findActivePermissionsForRole(String roleName);

    Optional<RolePermission> findByRole_IdAndPermission_Id(UUID roleId, UUID permissionId);
    @Query("""
        select rp from RolePermission rp
        where rp.active = true
    """)
    List<RolePermission> findAllActive();

    boolean existsByPermission_Id(UUID id);

    @Query("""
        select rp from RolePermission rp
        where rp.role.id = :roleId and rp.active = true
    """)
    List<RolePermission> findActiveByRoleId(@Param("roleId") UUID roleId);

    @Modifying
    @Query("""
        update RolePermission rp
        set rp.active = false
        where rp.role.id = :roleId
    """)
    int softDeleteByRoleId(@Param("roleId") UUID roleId);

    @Modifying
    @Query("""
        update RolePermission rp
        set rp.active = false
        where rp.id = :id
    """)
    void softDeleteById(@Param("id") UUID id);
}