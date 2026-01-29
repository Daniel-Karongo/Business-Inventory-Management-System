package com.IntegrityTechnologies.business_manager.security.acl.service;

import com.IntegrityTechnologies.business_manager.security.acl.entity.EndpointPermission;
import com.IntegrityTechnologies.business_manager.security.acl.entity.RolePermission;
import com.IntegrityTechnologies.business_manager.security.acl.repository.EndpointPermissionRepository;
import com.IntegrityTechnologies.business_manager.security.acl.repository.RolePermissionRepository;
import jakarta.annotation.PostConstruct;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

@Service
@RequiredArgsConstructor
public class PermissionCacheService {

    private final EndpointPermissionRepository endpointRepo;
    private final RolePermissionRepository rolePermissionRepo;

    private final Map<String, EndpointPermission> endpointCache = new ConcurrentHashMap<>();
    private final Map<String, Set<String>> rolePermissionCache = new ConcurrentHashMap<>();

    @PostConstruct
    public void loadCache() {
        refresh();
    }

    public synchronized void refresh() {

        endpointCache.clear();
        rolePermissionCache.clear();

        endpointRepo.findAllByActiveTrue().forEach(ep -> {
            String key = key(ep.getHttpMethod(), ep.getPath());
            endpointCache.put(key, ep);
        });

        rolePermissionRepo.findAllActive().forEach(rp -> {

            String roleName = rp.getRole().getName(); // ✅ FIXED
            String permCode = rp.getPermission().getCode();

            rolePermissionCache
                    .computeIfAbsent(roleName, k -> new HashSet<>())
                    .add(permCode);
        });
    }

    public Optional<EndpointPermission> getEndpoint(String method, String path) {
        return Optional.ofNullable(endpointCache.get(key(method, path)));
    }

    public boolean roleHasPermission(String roleName, String permissionCode) {
        return rolePermissionCache
                .getOrDefault(roleName, Set.of())
                .contains(permissionCode);
    }

    private String key(String method, String path) {
        return method.toUpperCase() + ":" + path;
    }
}