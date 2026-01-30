package com.IntegrityTechnologies.business_manager.security.acl.service;

import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.Role;
import com.IntegrityTechnologies.business_manager.security.acl.entity.*;
import com.IntegrityTechnologies.business_manager.security.acl.repository.*;
import com.IntegrityTechnologies.business_manager.security.acl.util.PathTemplateNormalizer;
import jakarta.annotation.PostConstruct;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.aop.support.AopUtils;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.DependsOn;
import org.springframework.core.annotation.AnnotatedElementUtils;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.stereotype.Service;
import org.springframework.web.bind.annotation.*;

import java.lang.annotation.Annotation;
import java.lang.reflect.Method;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

@Slf4j
@Service
@DependsOn("roleSeederService")
@RequiredArgsConstructor
public class PermissionSeederService {

    private final ApplicationContext context;
    private final PermissionRepository permissionRepo;
    private final EndpointPermissionRepository endpointRepo;
    private final RolePermissionRepository rolePermRepo;
    private final RoleEntityRepository roleRepo;
    private final PermissionConditionRepository conditionRepo;
    private final PermissionCacheService cache;

    private static final Pattern ROLE_PATTERN =
            Pattern.compile("hasAnyRole\\((.*?)\\)");

    private static final Pattern CONDITION_PATTERN =
            Pattern.compile("#(\\w+)\\s*==\\s*(true|false|null)");

    @PostConstruct
    public void seed() {

        log.info("🔐 ACL SEEDER START");

        Map<String, RoleEntity> roles = loadRoles();

        String[] controllers =
                context.getBeanNamesForAnnotation(RestController.class);

        for (String beanName : controllers) {

            Object bean = context.getBean(beanName);
            Class<?> clazz = AopUtils.getTargetClass(bean);

            RequestMapping base =
                    AnnotatedElementUtils.findMergedAnnotation(clazz, RequestMapping.class);

            String basePath =
                    (base != null && base.path().length > 0) ? base.path()[0] : "";

            for (Method method : clazz.getMethods()) {

                List<HttpMapping> mappings = resolveHttpMappings(method);
                if (mappings.isEmpty()) continue;

                PreAuthorize pre =
                        AnnotatedElementUtils.findMergedAnnotation(method, PreAuthorize.class);

                for (HttpMapping m : mappings) {

                    String rawPath = normalize(basePath + m.path);
                    String path = PathTemplateNormalizer.normalize(rawPath);

                    Permission permission = permissionRepo
                            .findByCodeIgnoreCase(buildPermissionCode(m.method, path))
                            .orElseGet(() -> permissionRepo.save(
                                    Permission.builder()
                                            .code(buildPermissionCode(m.method, path))
                                            .description(m.method + " " + path)
                                            .active(true)
                                            .build()
                            ));

                    endpointRepo.findByHttpMethodIgnoreCaseAndPathAndActiveTrue(m.method, path)
                            .orElseGet(() -> endpointRepo.save(
                                    EndpointPermission.builder()
                                            .httpMethod(m.method)
                                            .path(path)
                                            .permission(permission)
                                            .active(true)
                                            .build()
                            ));

                    // 🔒 CHECK BEFORE SUPERUSER
                    boolean alreadyConfigured =
                            rolePermRepo.existsByPermission_Id(permission.getId());

                    // SUPERUSER always allowed
                    ensureRoleGrant(permission, roles.get("SUPERUSER"));

                    if (alreadyConfigured) {
                        log.debug("🔒 Locked permission {}", permission.getCode());
                        continue;
                    }

                    if (pre == null) {
                        assignDefaultEmployee(permission, roles);
                    } else {
                        parseAndAssign(pre.value(), permission, roles);
                    }
                }
            }
        }

        log.info("✅ ACL SEEDER DONE");
        cache.refresh();
        log.info("🧠 ACL cache refreshed");
    }

    /* ===================== ROLE HANDLING ===================== */

    private Map<String, RoleEntity> loadRoles() {
        Map<String, RoleEntity> map = new HashMap<>();
        for (Role r : Role.values()) {
            map.put(
                    r.name(),
                    roleRepo.findByNameIgnoreCase(r.name())
                            .orElseThrow(() -> new IllegalStateException("Missing role " + r.name()))
            );
        }
        return map;
    }

    private void ensureRoleGrant(Permission p, RoleEntity r) {
        rolePermRepo.findByRole_IdAndPermission_Id(r.getId(), p.getId())
                .orElseGet(() -> rolePermRepo.save(
                        RolePermission.builder()
                                .role(r)
                                .permission(p)
                                .allowed(true)
                                .active(true)
                                .grantedBy("ACL_SEEDER")
                                .build()
                ));
    }

    private void assignDefaultEmployee(Permission p, Map<String, RoleEntity> roles) {
        roles.forEach((name, role) -> {
            if (Role.valueOf(name).getLevel() >= Role.EMPLOYEE.getLevel()) {
                ensureRoleGrant(p, role);
            }
        });
    }

    /* ===================== PARSING ===================== */

    private void parseAndAssign(
            String spel,
            Permission permission,
            Map<String, RoleEntity> roles
    ) {

        String[] orBlocks = spel.split("\\s+or\\s+");

        for (String block : orBlocks) {

            // 1️⃣ Extract conditions (mandatory)
            Matcher cm = CONDITION_PATTERN.matcher(block);
            List<String[]> conditions = new ArrayList<>();

            while (cm.find()) {
                conditions.add(new String[]{cm.group(1), cm.group(2)});
            }

            // 2️⃣ Determine target roles
            Set<RoleEntity> targetRoles = new HashSet<>();

            Matcher rm = ROLE_PATTERN.matcher(block);

            if (rm.find()) {
                // Explicit roles
                String[] roleNames = rm.group(1).replace("'", "").split(",");
                for (String roleName : roleNames) {
                    RoleEntity role = roles.get(roleName.trim());
                    if (role != null) {
                        targetRoles.add(role);
                    }
                }
            } else {
                // 👇 NO hasAnyRole → EMPLOYEE and above
                roles.forEach((name, role) -> {
                    if (Role.valueOf(name).getLevel() >= Role.EMPLOYEE.getLevel()) {
                        targetRoles.add(role);
                    }
                });
            }

            // 3️⃣ Persist grants + conditions
            for (RoleEntity role : targetRoles) {

                ensureRoleGrant(permission, role);

                for (String[] c : conditions) {
                    conditionRepo.save(
                            PermissionCondition.builder()
                                    .permission(permission)
                                    .role(role)
                                    .param(c[0])
                                    .operator("EQ")
                                    .value(c[1])
                                    .active(true)
                                    .build()
                    );
                }
            }
        }
    }

    /* ===================== HTTP ===================== */

    private List<HttpMapping> resolveHttpMappings(Method method) {

        List<HttpMapping> list = new ArrayList<>();

        extract(GetMapping.class, method, "GET", list);
        extract(PostMapping.class, method, "POST", list);
        extract(PatchMapping.class, method, "PATCH", list);
        extract(PutMapping.class, method, "PUT", list);
        extract(DeleteMapping.class, method, "DELETE", list);

        return list;
    }

    private void extract(
            Class<? extends Annotation> type,
            Method method,
            String verb,
            List<HttpMapping> out
    ) {
        Annotation ann =
                AnnotatedElementUtils.findMergedAnnotation(method, type);
        if (ann == null) return;

        try {
            String[] path =
                    (String[]) type.getMethod("path").invoke(ann);
            if (path.length == 0) {
                path = (String[]) type.getMethod("value").invoke(ann);
            }

            // ✅ CRITICAL FIX
            if (path.length == 0) {
                // root mapping
                out.add(new HttpMapping(verb, ""));
                return;
            }

            for (String p : path) {
                out.add(new HttpMapping(verb, p));
            }
        } catch (Exception e) {
            throw new IllegalStateException("Failed to extract mapping", e);
        }
    }

    private String normalize(String p) {
        if (!p.startsWith("/")) {
            p = "/" + p;
        }
        return p.replaceAll("//+", "/");
    }

    private String buildPermissionCode(String verb, String path) {

        String cleaned =
                path.startsWith("/") ? path.substring(1) : path;

        return "API_" + verb + "_" +
                cleaned
                        .replace("/", "_")
                        .replace("{", "")
                        .replace("}", "")
                        .replaceAll("_+", "_")
                        .toUpperCase();
    }

    private record HttpMapping(String method, String path) {}
}