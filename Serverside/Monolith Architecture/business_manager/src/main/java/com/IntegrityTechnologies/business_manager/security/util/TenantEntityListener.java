package com.IntegrityTechnologies.business_manager.security.util;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.model.TenantAwareEntity;
import jakarta.persistence.PrePersist;
import jakarta.persistence.PreUpdate;

import java.util.UUID;

public class TenantEntityListener {

    @PrePersist
    public void applyTenant(Object entity) {

        if (!(entity instanceof TenantAwareEntity tenantAware)) {
            return;
        }

        if (tenantAware.getTenantId() == null) {
            tenantAware.setTenantId(TenantContext.getTenantId());
        }
    }

    @PreUpdate
    public void protectTenant(Object entity) {

        if (!(entity instanceof TenantAwareEntity tenantAware)) {
            return;
        }

        UUID current = TenantContext.getOrNull();

        if (current == null) {
            return;
        }

        if (!current.equals(tenantAware.getTenantId())) {
            throw new SecurityException(
                    "Cross-tenant modification attempt"
            );
        }
    }
}