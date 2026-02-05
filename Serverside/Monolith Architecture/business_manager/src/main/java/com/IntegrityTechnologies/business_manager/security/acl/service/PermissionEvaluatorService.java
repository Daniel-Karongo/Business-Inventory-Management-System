package com.IntegrityTechnologies.business_manager.security.acl.service;

import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.Role;
import com.IntegrityTechnologies.business_manager.security.SecurityUtils;
import com.IntegrityTechnologies.business_manager.security.acl.entity.EndpointPermission;
import com.IntegrityTechnologies.business_manager.security.acl.entity.PermissionCondition;
import com.IntegrityTechnologies.business_manager.security.acl.entity.RoleEntity;
import com.IntegrityTechnologies.business_manager.security.acl.repository.PermissionConditionRepository;
import com.IntegrityTechnologies.business_manager.security.acl.repository.RoleEntityRepository;
import jakarta.annotation.PostConstruct;
import jakarta.servlet.http.HttpServletRequest;
import lombok.RequiredArgsConstructor;
import org.springframework.security.core.Authentication;
import org.springframework.stereotype.Service;
import org.springframework.util.AntPathMatcher;
import org.springframework.web.context.request.*;

import java.util.*;

@Service
@RequiredArgsConstructor
public class PermissionEvaluatorService {

    private final PermissionCacheService cache;
    private final PermissionConditionRepository conditionRepo;
    private final RoleEntityRepository roleRepo;

    private final AntPathMatcher matcher = new AntPathMatcher();

    @PostConstruct
    public void refreshCache() {
        cache.refresh();
    }
    /* =====================================================
       ENDPOINT RESOLUTION
       ===================================================== */
    public Optional<EndpointPermission> resolveEndpointPermission(
            String method,
            String requestPath
    ) {

        for (EndpointPermission ep : cache.getEndpoints()) {

            if (!ep.getHttpMethod().equalsIgnoreCase(method)) continue;

            // Convert {var} → *
            String pattern =
                    ep.getPath().replaceAll("\\{[^/]+}", "*");

            if (matcher.match(pattern, requestPath)) {
                return Optional.of(ep);
            }
        }

        return Optional.empty();
    }

    /* =====================================================
       CORE AUTHORIZATION
       ===================================================== */
    public boolean evaluatePermission(EndpointPermission ep) {

        Role role = SecurityUtils.currentRole();

        // 1️⃣ SUPERUSER bypass
        if (role == Role.SUPERUSER) {
            return true;
        }

        // 2️⃣ ROLE ACTIVE GUARD
        RoleEntity roleEntity =
                roleRepo.findByNameIgnoreCase(role.name()).orElse(null);

        if (roleEntity == null || !roleEntity.isActive()) {
            return false;
        }

        // 3️⃣ ENDPOINT ACTIVE GUARD
        if (!ep.isActive()) {
            return false;
        }

        // 4️⃣ PERMISSION ACTIVE GUARD
        if (!ep.getPermission().isActive()) {
            return false;
        }

        String permCode = ep.getPermission().getCode();

        // 5️⃣ ROLE → PERMISSION GRANT
        if (!cache.roleHasPermission(role.name(), permCode)) {
            return false;
        }

        // 6️⃣ CONDITIONS
        List<PermissionCondition> conditions =
                conditionRepo.findByPermission_IdAndRole_NameIgnoreCaseAndActiveTrue(
                        ep.getPermission().getId(), role.name()
                );

        // No conditions → allow
        if (conditions.isEmpty()) {
            return true;
        }

        HttpServletRequest req = currentRequest();
        if (req == null) return false;

        // OR semantics across conditions
        for (PermissionCondition c : conditions) {
            if (evaluateCondition(c, req)) {
                return true;
            }
        }

        return false;
    }

    /* =====================================================
       CONDITION EVALUATION
       ===================================================== */
    private boolean evaluateCondition(
            PermissionCondition c,
            HttpServletRequest req
    ) {
        String actual = req.getParameter(c.getParam());
        String expected = c.getValue();
        return Objects.equals(actual, expected);
    }

    private HttpServletRequest currentRequest() {
        ServletRequestAttributes a =
                (ServletRequestAttributes) RequestContextHolder.getRequestAttributes();
        return a != null ? a.getRequest() : null;
    }

    /* =====================================================
       OPTIONAL METHOD SECURITY
       ===================================================== */
    public boolean hasPermission(Authentication auth, String permissionCode) {
        Role role = SecurityUtils.currentRole();
        return role == Role.SUPERUSER
                || cache.roleHasPermission(role.name(), permissionCode);
    }
}