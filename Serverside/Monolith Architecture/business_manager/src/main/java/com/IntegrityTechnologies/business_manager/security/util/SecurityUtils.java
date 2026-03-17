package com.IntegrityTechnologies.business_manager.security.util;

import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.Role;
import com.IntegrityTechnologies.business_manager.security.auth.config.CustomUserDetails;
import com.IntegrityTechnologies.business_manager.security.auth.config.PlatformUserDetails;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.context.SecurityContextHolder;

import java.util.UUID;

public final class SecurityUtils {

    private SecurityUtils() {}

    /* ============================================================
       AUTHENTICATION
    ============================================================ */

    public static Authentication currentAuthentication() {
        try {
            return SecurityContextHolder.getContext().getAuthentication();
        } catch (Exception e) {
            return null;
        }
    }
    /* ============================================================
       USER INFO
    ============================================================ */

    public static String currentUsername() {

        try {

            Authentication auth = currentAuthentication();

            if (auth == null) return "system";

            Object principal = auth.getPrincipal();

            if (principal instanceof CustomUserDetails cud) {
                return cud.getUsername();
            }

            if (principal instanceof PlatformUserDetails pud) {
                return pud.getUsername();
            }

            if (principal instanceof String s) {
                return s;
            }

            return auth.getName();

        } catch (Exception e) {
            return "system";
        }

    }

    public static boolean isPlatformAdmin() {

        try {

            Authentication auth = currentAuthentication();

            if (auth == null) return false;

            Object principal = auth.getPrincipal();

            return principal instanceof PlatformUserDetails;

        } catch (Exception e) {
            return false;
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

    public static UUID currentUserId() {

        try {

            Authentication auth = currentAuthentication();

            if (auth == null) return null;

            Object principal = auth.getPrincipal();

            if (principal instanceof CustomUserDetails cud) {
                return cud.getId();
            }

            if (principal instanceof PlatformUserDetails pud) {
                return pud.getId();
            }

            return null;

        } catch (Exception e) {
            return null;
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

    public static boolean isSuperuser() {
        return currentRole() == Role.SUPERUSER;
    }

    public static boolean isAdmin() {
        return currentRole().canAccess(Role.ADMIN);
    }

    public static boolean isManager() {
        return currentRole().canAccess(Role.MANAGER);
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