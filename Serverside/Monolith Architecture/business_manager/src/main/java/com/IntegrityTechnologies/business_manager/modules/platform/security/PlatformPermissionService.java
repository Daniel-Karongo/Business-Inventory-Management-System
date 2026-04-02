package com.IntegrityTechnologies.business_manager.modules.platform.security;

import com.IntegrityTechnologies.business_manager.modules.person.user.model.Role;
import com.IntegrityTechnologies.business_manager.security.auth.config.PlatformUserDetails;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import com.IntegrityTechnologies.business_manager.security.util.SecurityUtils;
import org.springframework.security.core.Authentication;
import org.springframework.stereotype.Component;

@Component("platformSecurity")
public class PlatformPermissionService {

    /* =========================================================
       PLATFORM LEVEL
    ========================================================= */

    public boolean isPlatformAdmin() {

        Authentication auth = SecurityUtils.currentAuthentication();

        if (auth == null) return false;

        Object principal = auth.getPrincipal();

        return principal instanceof PlatformUserDetails;

    }

    /* =========================================================
       TENANT LEVEL
    ========================================================= */

    public boolean isTenantAdmin() {

        Role role = SecurityUtils.currentRole();

        return role == Role.SUPERUSER
                || role == Role.ADMIN;

    }

    public boolean isTenantManager() {

        Role role = SecurityUtils.currentRole();

        return role == Role.SUPERUSER
                || role == Role.ADMIN
                || role == Role.MANAGER;

    }

    public boolean isTenantUser() {

        Role role = SecurityUtils.currentRole();

        return role != null;

    }

    /* =========================================================
       TENANT CONTEXT SAFETY
    ========================================================= */

    public boolean hasTenantContext() {

        return TenantContext.getOrNull() != null;

    }

}