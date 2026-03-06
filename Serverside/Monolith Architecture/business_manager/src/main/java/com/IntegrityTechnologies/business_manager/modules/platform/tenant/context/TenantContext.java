package com.IntegrityTechnologies.business_manager.modules.platform.tenant.context;

import java.util.UUID;

public final class TenantContext {

    private static final InheritableThreadLocal<UUID> CURRENT_TENANT =
            new InheritableThreadLocal<>();

    private TenantContext(){}

    public static void setTenantId(UUID tenantId){
        CURRENT_TENANT.set(tenantId);
    }

    public static UUID getTenantId(){

        UUID tenantId = CURRENT_TENANT.get();

        if (tenantId == null) {
            throw new IllegalStateException(
                    "TenantContext not initialized for this thread"
            );
        }

        return tenantId;
    }

    public static UUID getOrNull(){
        return CURRENT_TENANT.get();
    }

    public static void clear(){
        CURRENT_TENANT.remove();
    }

}