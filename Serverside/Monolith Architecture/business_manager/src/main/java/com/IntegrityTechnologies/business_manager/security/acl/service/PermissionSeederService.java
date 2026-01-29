package com.IntegrityTechnologies.business_manager.security.acl.service;

import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.Role;
import com.IntegrityTechnologies.business_manager.security.acl.entity.*;
import com.IntegrityTechnologies.business_manager.security.acl.repository.*;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.aop.support.AopUtils;
import org.springframework.context.ApplicationContext;
import org.springframework.core.annotation.AnnotatedElementUtils;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.stereotype.Service;
import org.springframework.web.bind.annotation.*;

import jakarta.annotation.PostConstruct;
import java.lang.annotation.Annotation;
import java.lang.reflect.Method;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

@Slf4j
@Service
@RequiredArgsConstructor
public class PermissionSeederService {

    private final ApplicationContext context;
    private final PermissionRepository permissionRepo;
    private final EndpointPermissionRepository endpointRepo;
    private final RolePermissionRepository rolePermRepo;
    private final RoleEntityRepository roleRepo;
    private final PermissionConditionRepository conditionRepo;

    private static final Pattern ROLE_PATTERN =
            Pattern.compile("hasAnyRole\\((.*?)\\)");

    private static final Pattern CONDITION_PATTERN =
            Pattern.compile("(#\\w+\\s*==\\s*(true|false|null))");

    @PostConstruct
    public void seed() {

        log.info("🔐 ACL SAFE LOCKED SEEDER running...");

        Map<String, RoleEntity> roleMap = new HashMap<>();
        for (Role r : Role.values()) {
            roleMap.put(r.name(),
                    roleRepo.findByNameIgnoreCase(r.name())
                            .orElseThrow(() -> new IllegalStateException("Missing ACL role: " + r.name())));
        }

        Set<String> discoveredEndpoints = new HashSet<>();
        Set<String> discoveredPermissions = new HashSet<>();

        String[] controllers = context.getBeanNamesForAnnotation(RestController.class);

        for (String beanName : controllers) {

            Object bean = context.getBean(beanName);
            Class<?> clazz = AopUtils.getTargetClass(bean);

            RequestMapping base =
                    AnnotatedElementUtils.findMergedAnnotation(clazz, RequestMapping.class);

            String basePath = (base != null && base.path().length > 0)
                    ? base.path()[0] : "";

            for (Method method : clazz.getMethods()) {

                List<HttpMapping> mappings = resolveHttpMappings(method);
                if (mappings.isEmpty()) continue;

                PreAuthorize pre =
                        AnnotatedElementUtils.findMergedAnnotation(method, PreAuthorize.class);

                for (HttpMapping map : mappings) {

                    String fullPath = normalize(basePath + map.path);
                    String httpMethod = map.method;

                    String endpointKey = httpMethod + ":" + fullPath;
                    discoveredEndpoints.add(endpointKey);

                    String permissionCode = buildPermissionCode(clazz, method, httpMethod, fullPath);
                    discoveredPermissions.add(permissionCode);

                    Permission permission = permissionRepo.findByCodeIgnoreCase(permissionCode)
                            .orElseGet(() -> {
                                Permission p = permissionRepo.save(
                                        Permission.builder()
                                                .code(permissionCode)
                                                .description(httpMethod + " " + fullPath)
                                                .active(true)
                                                .build()
                                );
                                log.info("✅ Created permission {}", permissionCode);
                                return p;
                            });

                    endpointRepo.findActiveByMethodAndPath(httpMethod, fullPath)
                            .orElseGet(() -> {
                                EndpointPermission ep = endpointRepo.save(
                                        EndpointPermission.builder()
                                                .httpMethod(httpMethod)
                                                .path(fullPath)
                                                .permission(permission)
                                                .active(true)
                                                .build()
                                );
                                log.info("🔗 Bound {} {}", httpMethod, fullPath);
                                return ep;
                            });

                    // SUPERUSER always allowed — ensure grant exists
                    ensureRoleGrant(permission, roleMap.get("SUPERUSER"));

                    boolean permissionAlreadyConfigured =
                            rolePermRepo.existsByPermission_Id(permission.getId());

                    // 🚨 LOCK RULE: DO NOT override existing grants
                    if (permissionAlreadyConfigured) {
                        log.debug("🔒 Preserved ACL rules for {}", permissionCode);
                        continue;
                    }

                    // If NEW permission → seed rules
                    if (pre == null) {
                        assignDefaultEmployee(permission, roleMap);
                    } else {
                        parseAndAssign(pre.value(), permission, roleMap);
                    }
                }
            }
        }

        cleanupStaleEndpoints(discoveredEndpoints);
        cleanupUnusedPermissions(discoveredPermissions);

        log.info("✅ ACL SAFE LOCKED SEEDER finished");
    }

    /* ===================== ROLE ASSIGNMENT ===================== */

    private void assignDefaultEmployee(Permission permission, Map<String, RoleEntity> roles) {
        roles.forEach((name, role) -> {
            if (Role.valueOf(name).getLevel() >= Role.EMPLOYEE.getLevel()) {
                ensureRoleGrant(permission, role);
            }
        });
    }

    private void ensureRoleGrant(Permission permission, RoleEntity role) {
        rolePermRepo.findByRole_IdAndPermission_Id(role.getId(), permission.getId())
                .orElseGet(() -> rolePermRepo.save(
                        RolePermission.builder()
                                .role(role)
                                .permission(permission)
                                .allowed(true)
                                .active(true)
                                .grantedBy("ACL_SEEDER")
                                .build()
                ));
    }

    private void parseAndAssign(String spel, Permission permission, Map<String, RoleEntity> roles) {

        Matcher roleMatch = ROLE_PATTERN.matcher(spel);
        Set<String> foundRoles = new HashSet<>();

        while (roleMatch.find()) {
            String[] parsed = roleMatch.group(1).replace("'", "").split(",");
            for (String r : parsed) foundRoles.add(r.trim());
        }

        Matcher condMatch = CONDITION_PATTERN.matcher(spel);
        List<String> conditions = new ArrayList<>();

        while (condMatch.find()) {
            conditions.add(condMatch.group(1));
        }

        for (String roleName : foundRoles) {

            RoleEntity role = roles.get(roleName);
            if (role == null) continue;

            ensureRoleGrant(permission, role);

            for (String cond : conditions) {
                conditionRepo.save(
                        PermissionCondition.builder()
                                .role(role)
                                .permission(permission)
                                .expression(cond)
                                .active(true)
                                .build()
                );
            }
        }
    }

    /* ===================== HTTP RESOLUTION ===================== */

    private List<HttpMapping> resolveHttpMappings(Method method) {
        List<HttpMapping> mappings = new ArrayList<>();

        extract(GetMapping.class, method, "GET", mappings);
        extract(PostMapping.class, method, "POST", mappings);
        extract(PutMapping.class, method, "PUT", mappings);
        extract(PatchMapping.class, method, "PATCH", mappings);
        extract(DeleteMapping.class, method, "DELETE", mappings);

        RequestMapping rm = AnnotatedElementUtils.findMergedAnnotation(method, RequestMapping.class);
        if (rm != null) {
            for (RequestMethod m : rm.method()) {
                add(mappings, m.name(), rm.path(), rm.value());
            }
        }

        return mappings;
    }

    private void extract(Class<? extends Annotation> type, Method method, String verb, List<HttpMapping> out) {
        Annotation ann = AnnotatedElementUtils.findMergedAnnotation(method, type);
        if (ann == null) return;

        try {
            String[] path = (String[]) type.getMethod("path").invoke(ann);
            String[] value = (String[]) type.getMethod("value").invoke(ann);
            add(out, verb, path, value);
        } catch (Exception ignored) {}
    }

    private void add(List<HttpMapping> list, String method, String[] path, String[] value) {
        String[] arr = path.length > 0 ? path : value;
        if (arr.length == 0) arr = new String[]{""};
        for (String p : arr) list.add(new HttpMapping(method, p));
    }

    private String normalize(String p) {
        return p.replaceAll("//+", "/");
    }

    private String buildPermissionCode(Class<?> c, Method m, String verb, String path) {
        return "API_" + verb + "_" +
                path.replace("/", "_")
                        .replace("{", "")
                        .replace("}", "")
                        .replace("-", "_")
                        .toUpperCase();
    }

    /* ===================== CLEANUP ===================== */

    private void cleanupStaleEndpoints(Set<String> discovered) {
        log.info("🧹 Cleaning stale endpoints...");

        endpointRepo.findAll().forEach(ep -> {
            String key = ep.getHttpMethod() + ":" + ep.getPath();

            if (!discovered.contains(key)) {
                ep.setActive(false);
                endpointRepo.save(ep);
                log.warn("⚠️ Disabled stale endpoint {} {}", ep.getHttpMethod(), ep.getPath());
            }
        });
    }

    private void cleanupUnusedPermissions(Set<String> activeCodes) {
        log.info("🧹 Cleaning unused permissions...");

        permissionRepo.findAll().forEach(p -> {
            if (!activeCodes.contains(p.getCode())) {
                p.setActive(false);
                permissionRepo.save(p);
                log.warn("⚠️ Disabled unused permission {}", p.getCode());
            }
        });
    }

    private record HttpMapping(String method, String path) {}
}