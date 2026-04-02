//package com.IntegrityTechnologies.business_manager.modules.acl.service;
//
//import com.IntegrityTechnologies.business_manager.modules.person.model.user.Role;
//import com.IntegrityTechnologies.business_manager.security.util.SecurityUtils;
//import com.IntegrityTechnologies.business_manager.modules.acl.entity.EndpointPermission;
//import com.IntegrityTechnologies.business_manager.modules.acl.entity.PermissionCondition;
//import com.IntegrityTechnologies.business_manager.modules.acl.entity.RoleEntity;
//import com.IntegrityTechnologies.business_manager.modules.acl.repository.PermissionConditionRepository;
//import com.IntegrityTechnologies.business_manager.modules.acl.repository.RoleEntityRepository;
//import jakarta.annotation.PostConstruct;
//import jakarta.servlet.http.HttpServletRequest;
//import lombok.RequiredArgsConstructor;
//import org.springframework.security.core.Authentication;
//import org.springframework.stereotype.Service;
//import org.springframework.util.AntPathMatcher;
//import org.springframework.web.context.request.*;
//
//import java.util.*;
//
//@Service
//@RequiredArgsConstructor
//public class PermissionEvaluatorService {
//
//    private final PermissionCacheService cache;
//    private final PermissionConditionRepository conditionRepo;
//    private final RoleEntityRepository roleRepo;
//
//    private final AntPathMatcher matcher = new AntPathMatcher();
//
//    @PostConstruct
//    public void refreshCache() {
//        cache.refresh();
//    }
//    /* =====================================================
//       ENDPOINT RESOLUTION
//       ===================================================== */
//    public Optional<EndpointPermission> resolveEndpointPermission(
//            String method,
//            String requestPath
//    ) {
//
//        // 1️⃣ Exact match (O(1))
//        EndpointPermission exact =
//                cache.getExactEndpoint(method, requestPath);
//
//        if (exact != null) {
//            return Optional.of(exact);
//        }
//
//        // 2️⃣ Pattern fallback
//        for (EndpointPermission ep : cache.getPatternEndpoints()) {
//
//            if (!ep.getHttpMethod().equalsIgnoreCase(method)) {
//                continue;
//            }
//
//            String pattern =
//                    ep.getPath().replaceAll("\\{[^/]+}", "*");
//
//            if (matcher.match(pattern, requestPath)) {
//                return Optional.of(ep);
//            }
//        }
//
//        return Optional.empty();
//    }
//
//    /* =====================================================
//       CORE AUTHORIZATION
//       ===================================================== */
//    public boolean evaluatePermission(EndpointPermission ep) {
//
//        Role role = SecurityUtils.currentRole();
//
//        // 1️⃣ SUPERUSER bypass
//        if (role == Role.SUPERUSER) {
//            return true;
//        }
//
//        // 2️⃣ ROLE ACTIVE GUARD
//        RoleEntity roleEntity =
//                roleRepo.findByName(role).orElse(null);
//
//        if (roleEntity == null || !roleEntity.isActive()) {
//            return false;
//        }
//
//        // 3️⃣ ENDPOINT ACTIVE GUARD
//        if (!ep.isActive()) {
//            return false;
//        }
//
//        // 4️⃣ PERMISSION ACTIVE GUARD
//        if (!ep.getPermission().isActive()) {
//            return false;
//        }
//
//        String permCode = ep.getPermission().getCode();
//
//        // 5️⃣ ROLE → PERMISSION GRANT
//        if (!cache.roleHasPermission(role, permCode)) {
//            return false;
//        }
//
//        // 6️⃣ CONDITIONS
//        List<PermissionCondition> conditions =
//                conditionRepo.findByPermission_IdAndRole_NameAndActiveTrue(
//                        ep.getPermission().getId(), role
//                );
//
//        // No conditions → allow
//        if (conditions.isEmpty()) {
//            return true;
//        }
//
//        HttpServletRequest req = currentRequest();
//        if (req == null) return false;
//
//        // OR semantics across conditions
//        for (PermissionCondition c : conditions) {
//            if (evaluateCondition(c, req)) {
//                return true;
//            }
//        }
//
//        return false;
//    }
//
//    /* =====================================================
//       CONDITION EVALUATION
//       ===================================================== */
//    private boolean evaluateCondition(
//            PermissionCondition c,
//            HttpServletRequest req
//    ) {
//        String actual = req.getParameter(c.getParam());
//        String expected = c.getValue();
//        return Objects.equals(actual, expected);
//    }
//
//    private HttpServletRequest currentRequest() {
//        ServletRequestAttributes a =
//                (ServletRequestAttributes) RequestContextHolder.getRequestAttributes();
//        return a != null ? a.getRequest() : null;
//    }
//}