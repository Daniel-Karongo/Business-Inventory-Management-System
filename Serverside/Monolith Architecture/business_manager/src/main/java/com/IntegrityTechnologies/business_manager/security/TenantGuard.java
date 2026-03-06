package com.IntegrityTechnologies.business_manager.security;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.context.TenantContext;

import java.util.UUID;

public final class TenantGuard {

    private TenantGuard(){}

    public static void assertTenant(UUID tenantId) {

        UUID current = TenantContext.getTenantId();

        if (!current.equals(tenantId)) {
            throw new SecurityException("Cross-tenant access denied");
        }

    }
}