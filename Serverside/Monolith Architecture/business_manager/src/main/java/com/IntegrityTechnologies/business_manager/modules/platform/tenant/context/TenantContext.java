package com.IntegrityTechnologies.business_manager.modules.platform.tenant.context;

import java.util.UUID;

public final class TenantContext {

    private static final ThreadLocal<UUID> TENANT = new ThreadLocal<>();

    private TenantContext(){}

    public static void setTenantId(UUID tenantId){
        TENANT.set(tenantId);
    }

    public static UUID getTenantId(){

        UUID tenantId = TENANT.get();

        if (tenantId == null) {
            throw new IllegalStateException(
                    "TenantContext not initialized for this thread"
            );
        }

        return tenantId;
    }

    public static UUID getOrNull(){
        return TENANT.get();
    }

    public static void clear(){
        TENANT.remove();
    }

    public static UUID getTenantIdOrNull() {

        try {
            return getTenantId();
        } catch (Exception e) {
            return null;
        }

    }
}