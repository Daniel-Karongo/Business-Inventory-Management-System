package com.IntegrityTechnologies.business_manager.security;

import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.Role;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.context.SecurityContextHolder;

public final class SecurityUtils {

    private SecurityUtils() {}

    public static String currentUsername() {
        try {
            Authentication auth = SecurityContextHolder.getContext().getAuthentication();
            if (auth == null) return "system";
            Object p = auth.getPrincipal();
            if (p instanceof org.springframework.security.core.userdetails.UserDetails) {
                return ((org.springframework.security.core.userdetails.UserDetails) p).getUsername();
            } else if (p instanceof String) {
                return (String) p;
            } else {
                return auth.getName() != null ? auth.getName() : "system";
            }
        } catch (Exception e) {
            return "system";
        }
    }

    public static Role currentRole() {
        try {
            Authentication auth = SecurityContextHolder.getContext().getAuthentication();
            if (auth == null || auth.getAuthorities() == null || auth.getAuthorities().isEmpty()) {
                return Role.EMPLOYEE; // default role
            }

            GrantedAuthority authority = auth.getAuthorities().iterator().next();
            String roleName = authority.getAuthority();

            // Remove "ROLE_" prefix if present
            if (roleName.startsWith("ROLE_")) {
                roleName = roleName.substring(5);
            }

            return Role.valueOf(roleName);

        } catch (Exception e) {
            return Role.EMPLOYEE; // fallback
        }
    }
}