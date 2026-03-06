package com.IntegrityTechnologies.business_manager.modules.platform.tenant.listener;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.context.TenantContext;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.model.TenantAwareEntity;
import jakarta.persistence.PrePersist;

import java.util.UUID;

public class TenantEntityListener {

    @PrePersist
    public void applyTenant(Object entity) {

        if (!(entity instanceof TenantAwareEntity tenantAware)) {
            return;
        }

        if (tenantAware.getTenantId() != null) {
            return;
        }

        UUID tenantId = TenantContext.getTenantId();

        tenantAware.setTenantId(tenantId);
    }

}