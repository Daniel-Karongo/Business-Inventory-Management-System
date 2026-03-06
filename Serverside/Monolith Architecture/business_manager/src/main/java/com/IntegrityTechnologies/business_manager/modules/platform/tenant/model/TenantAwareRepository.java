package com.IntegrityTechnologies.business_manager.modules.platform.tenant.model;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.context.TenantContext;

import java.util.UUID;

public abstract class TenantAwareRepository {

    protected UUID tenantId() {
        return TenantContext.getTenantId();
    }

}