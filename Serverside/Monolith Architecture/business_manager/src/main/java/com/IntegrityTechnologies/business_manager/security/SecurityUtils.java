package com.IntegrityTechnologies.business_manager.security;

import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.Role;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.context.SecurityContextHolder;

public final class SecurityUtils {

    private SecurityUtils() {}

    /* ============================================================
       USER INFO
    ============================================================ */

    public static String currentUsername() {
        try {
            Authentication auth = SecurityContextHolder.getContext().getAuthentication();
            if (auth == null) return "system";
            Object p = auth.getPrincipal();

            if (p instanceof org.springframework.security.core.userdetails.UserDetails u) {
                return u.getUsername();
            }
            if (p instanceof String s) {
                return s;
            }
            return auth.getName() != null ? auth.getName() : "system";

        } catch (Exception e) {
            return "system";
        }
    }

    public static Role currentRole() {
        try {
            Authentication auth = SecurityContextHolder.getContext().getAuthentication();
            if (auth == null || auth.getAuthorities() == null || auth.getAuthorities().isEmpty()) {
                return Role.EMPLOYEE;
            }

            GrantedAuthority authority = auth.getAuthorities().iterator().next();
            String roleName = authority.getAuthority();

            if (roleName.startsWith("ROLE_")) {
                roleName = roleName.substring(5);
            }

            return Role.valueOf(roleName);

        } catch (Exception e) {
            return Role.EMPLOYEE;
        }
    }

    /* ============================================================
       ROLE GUARDS (ENTERPRISE SAFE)
    ============================================================ */

    public static void requireAtLeast(Role required) {
        Role current = currentRole();
        if (!current.canAccess(required)) {
            throw new SecurityException(
                    "Access denied: requires " + required + ", current role is " + current
            );
        }
    }

    public static void requireAdmin() {
        requireAtLeast(Role.ADMIN);
    }

    public static void requireManager() {
        requireAtLeast(Role.MANAGER);
    }

    public static void requireSupervisor() {
        requireAtLeast(Role.SUPERVISOR);
    }

    public static boolean hasAtLeast(Role role) {
        return currentRole().canAccess(role);
    }
}