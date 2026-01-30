package com.IntegrityTechnologies.business_manager.security.acl.repository;

import com.IntegrityTechnologies.business_manager.security.acl.entity.Permission;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.Optional;
import java.util.UUID;

public interface PermissionRepository extends JpaRepository<Permission, UUID> {
    Optional<Permission> findByCodeIgnoreCase(String code);
    @Modifying
    @Query("""
        update Permission p
        set p.active = false
        where p.id = :id
    """)
    void softDelete(@Param("id") UUID id);
    boolean existsByCodeIgnoreCase(String code);
}