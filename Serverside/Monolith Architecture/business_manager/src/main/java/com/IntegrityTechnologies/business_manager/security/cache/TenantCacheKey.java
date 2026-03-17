package com.IntegrityTechnologies.business_manager.security.cache;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.context.TenantContext;

import java.util.UUID;

public final class TenantCacheKey {

    private TenantCacheKey(){}

    public static String user(String identifier) {

        UUID tenantId = TenantContext.getTenantId();

        return tenantId + "::user::" + identifier;
    }

    public static String allUsers(Boolean deleted, int page, int size) {
        UUID tenantId = TenantContext.getTenantId();
        return tenantId + ":users:" + deleted + ":" + page + ":" + size;
    }

}