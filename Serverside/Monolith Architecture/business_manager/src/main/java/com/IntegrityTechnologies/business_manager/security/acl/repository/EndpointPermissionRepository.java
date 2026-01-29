package com.IntegrityTechnologies.business_manager.security.acl.repository;

import com.IntegrityTechnologies.business_manager.security.acl.entity.EndpointPermission;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.util.List;
import java.util.Optional;

public interface EndpointPermissionRepository extends JpaRepository<EndpointPermission, Long> {
    @Query("""
        select ep 
        from EndpointPermission ep 
        join fetch ep.permission p
        where ep.httpMethod = :method
          and ep.path = :path
          and ep.active = true
          and p.active = true
    """)
    Optional<EndpointPermission> findActiveByMethodAndPath(String method, String path);

    Optional<EndpointPermission> findByHttpMethodIgnoreCaseAndPathAndActiveTrue(String method, String path);

    List<EndpointPermission> findAllByActiveTrue();
}