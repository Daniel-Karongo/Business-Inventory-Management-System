package com.IntegrityTechnologies.business_manager.modules.platform.security.service;

import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.Role;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.*;
import com.IntegrityTechnologies.business_manager.security.SecurityUtils;
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
         TENANT ROLES
         */

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