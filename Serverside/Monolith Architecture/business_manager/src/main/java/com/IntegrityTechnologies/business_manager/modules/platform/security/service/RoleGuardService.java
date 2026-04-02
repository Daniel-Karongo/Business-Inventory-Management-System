package com.IntegrityTechnologies.business_manager.modules.platform.security.service;

import com.IntegrityTechnologies.business_manager.modules.person.user.model.Role;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.*;
import com.IntegrityTechnologies.business_manager.security.util.SecurityUtils;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.lang.annotation.Annotation;

@Service
@Slf4j
public class RoleGuardService {

    public boolean isAllowed(Annotation annotation) {

        Role currentRole = SecurityUtils.currentRole();

        /*
         PLATFORM ADMIN ONLY
         */
        if (annotation instanceof PlatformAdminOnly) {
            return SecurityUtils.isPlatformAdmin();
        }

        if (annotation instanceof PlatformSuperAdminOnly) {
            return SecurityUtils.currentAuthentication()
                    .getAuthorities()
                    .stream()
                    .anyMatch(a -> a.getAuthority().equals("ROLE_PLATFORM_SUPER_ADMIN"));
        }

        /*
         HYBRID ROLES
         */
        if (annotation instanceof PlatformUserOrTenantManager) {

            if (SecurityUtils.isPlatformAdmin()) {
                return true;
            }

            Role role = SecurityUtils.currentRole();

            return role.canAccess(Role.MANAGER);

        }
        /*
         TENANT ROLES
         */
        if (annotation instanceof TenantSuperuserOnly) {
            return currentRole.canAccess(Role.SUPERUSER);
        }

        if (annotation instanceof TenantAdminOnly) {
            return currentRole.canAccess(Role.ADMIN);
        }

        if (annotation instanceof TenantManagerOnly) {
            return currentRole.canAccess(Role.MANAGER);
        }

        if (annotation instanceof TenantSupervisorOnly) {
            return currentRole.canAccess(Role.SUPERVISOR);
        }

        if (annotation instanceof TenantUserOnly) {
            return currentRole.canAccess(Role.EMPLOYEE);
        }

        return true;
    }
}