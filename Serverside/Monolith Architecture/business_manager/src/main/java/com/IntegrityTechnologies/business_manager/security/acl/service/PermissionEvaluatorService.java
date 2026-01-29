package com.IntegrityTechnologies.business_manager.security.acl.service;

import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.Role;
import com.IntegrityTechnologies.business_manager.security.SecurityUtils;
import com.IntegrityTechnologies.business_manager.security.acl.entity.EndpointPermission;
import com.IntegrityTechnologies.business_manager.security.acl.repository.UserBranchScopeRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.security.core.Authentication;
import org.springframework.stereotype.Service;

import java.util.Optional;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class PermissionEvaluatorService {

    private final PermissionCacheService cache;
    private final UserBranchScopeRepository branchScopeRepo;

    /* ================================
       HTTP FILTER LOOKUP
    ================================= */
    public Optional<EndpointPermission> resolveEndpointPermission(String method, String path) {
        return cache.getEndpoint(method, path);
    }

    /* ================================
       FILTER-LEVEL PERMISSION CHECK
    ================================= */
    public boolean evaluatePermission(EndpointPermission ep) {

        Role role = SecurityUtils.currentRole();

        if (role == Role.SUPERUSER) return true;

        String permissionCode = ep.getPermission().getCode();

        if (!cache.roleHasPermission(role.name(), permissionCode)) {
            return false;
        }

        UUID branchId = RequestContextBranchResolver.resolveBranchId();
        if (branchId == null) return true;

        UUID userId = SecurityUtils.currentUserId();

        return hasBranchAccess(userId, branchId);
    }

    /* ================================
       ASPECT PERMISSION CHECK
    ================================= */
    public boolean hasPermission(Authentication auth, String permissionCode) {

        Role role = SecurityUtils.currentRole();

        if (role == Role.SUPERUSER) return true;

        return cache.roleHasPermission(role.name(), permissionCode);
    }

    /* ================================
       BRANCH ACCESS CHECK
    ================================= */
    public boolean hasBranchAccess(UUID userId, UUID branchId) {

        if (userId == null || branchId == null) return false;

        return branchScopeRepo.existsByUserIdAndBranchIdAndActiveTrue(userId, branchId);
    }
}