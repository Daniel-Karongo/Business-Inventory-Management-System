//package com.IntegrityTechnologies.business_manager.modules.acl.service;
//
//import com.IntegrityTechnologies.business_manager.modules.acl.entity.EndpointPermission;
//import com.IntegrityTechnologies.business_manager.modules.acl.repository.EndpointPermissionRepository;
//import com.IntegrityTechnologies.business_manager.modules.acl.repository.RolePermissionRepository;
//import com.IntegrityTechnologies.business_manager.modules.person.model.user.Role;
//import lombok.RequiredArgsConstructor;
//import lombok.extern.slf4j.Slf4j;
//import org.springframework.stereotype.Service;
//
//import java.util.*;
//import java.util.concurrent.ConcurrentHashMap;
//
//@Service
//@Slf4j
//@RequiredArgsConstructor
//public class PermissionCacheService {
//
//    private final EndpointPermissionRepository endpointRepo;
//    private final RolePermissionRepository rolePermissionRepo;
//
//    /*
//     * METHOD:PATH → EndpointPermission
//     * Example:
//     * GET:/api/products
//     */
//    private final Map<String, EndpointPermission> exactEndpoints = new ConcurrentHashMap<>();
//
//    /*
//     * Pattern endpoints
//     * Example:
//     * /api/products/{id}
//     */
//    private final List<EndpointPermission> patternEndpoints = new ArrayList<>();
//
//    /*
//     * Role → Permission Codes
//     */
//    private final Map<Role, Set<String>> rolePermissionCache = new ConcurrentHashMap<>();
//
//    public synchronized void refresh() {
//
//        exactEndpoints.clear();
//        patternEndpoints.clear();
//        rolePermissionCache.clear();
//
//        List<EndpointPermission> endpoints = endpointRepo.findAllByActiveTrue();
//
//        for (EndpointPermission ep : endpoints) {
//
//            String key = buildKey(ep.getHttpMethod(), ep.getPath());
//
//            if (ep.getPath().contains("{")) {
//                patternEndpoints.add(ep);
//            } else {
//                exactEndpoints.put(key, ep);
//            }
//        }
//
//        rolePermissionRepo.findAllActive().forEach(rp ->
//                rolePermissionCache
//                        .computeIfAbsent(rp.getRole().getName(), r -> new HashSet<>())
//                        .add(rp.getPermission().getCode())
//        );
//
//        log.info("ACL cache refreshed | endpoints={} | rolePermissions={}",
//                endpoints.size(),
//                rolePermissionCache.size()
//        );
//    }
//
//    public EndpointPermission getExactEndpoint(String method, String path) {
//        return exactEndpoints.get(buildKey(method, path));
//    }
//
//    public List<EndpointPermission> getPatternEndpoints() {
//        return patternEndpoints;
//    }
//
//    public boolean roleHasPermission(Role role, String permissionCode) {
//
//        return rolePermissionCache
//                .getOrDefault(role, Collections.emptySet())
//                .contains(permissionCode);
//    }
//
//    private String buildKey(String method, String path) {
//        return method.toUpperCase() + ":" + path;
//    }
//}