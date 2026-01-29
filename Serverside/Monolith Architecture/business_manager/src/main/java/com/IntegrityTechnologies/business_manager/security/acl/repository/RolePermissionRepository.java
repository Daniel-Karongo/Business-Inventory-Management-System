package com.IntegrityTechnologies.business_manager.security.acl.repository;

import com.IntegrityTechnologies.business_manager.security.acl.entity.RolePermission;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

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
}