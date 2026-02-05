package com.IntegrityTechnologies.business_manager.security.acl.service;

import com.IntegrityTechnologies.business_manager.security.acl.entity.EndpointPermission;
import com.IntegrityTechnologies.business_manager.security.acl.repository.EndpointPermissionRepository;
import com.IntegrityTechnologies.business_manager.security.acl.repository.RolePermissionRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

@Service
@Slf4j
@RequiredArgsConstructor
public class PermissionCacheService {

    private final EndpointPermissionRepository endpointRepo;
    private final RolePermissionRepository rolePermissionRepo;

    private final List<EndpointPermission> endpoints = new ArrayList<>();
    private final Map<String, Set<String>> rolePermissionCache = new ConcurrentHashMap<>();

    public synchronized void refresh() {

        endpoints.clear();
        rolePermissionCache.clear();

        endpoints.addAll(endpointRepo.findAllByActiveTrue());

        rolePermissionRepo.findAllActive().forEach(rp ->
                rolePermissionCache
                        .computeIfAbsent(rp.getRole().getName(), k -> new HashSet<>())
                        .add(rp.getPermission().getCode())
        );
        log.info("🧠 ACL cache refreshed");
    }

    public List<EndpointPermission> getEndpoints() {
        return endpoints;
    }

    public boolean roleHasPermission(String roleName, String permissionCode) {
        return rolePermissionCache
                .getOrDefault(roleName, Set.of())
                .contains(permissionCode);
    }
}
