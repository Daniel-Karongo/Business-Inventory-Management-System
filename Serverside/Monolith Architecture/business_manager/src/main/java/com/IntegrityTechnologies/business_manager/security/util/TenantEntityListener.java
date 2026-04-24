package com.IntegrityTechnologies.business_manager.security.util;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.model.BranchAwareEntity;
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

        // 🔥 ADD THIS
        if (entity instanceof BranchAwareEntity branchAware) {
            if (branchAware.getBranchId() == null) {
                branchAware.setBranchId(BranchContext.get());
            }
        }
    }

    @PreUpdate
    public void protectTenant(Object entity) {

        if (!(entity instanceof TenantAwareEntity tenantAware)) {
            return;
        }

    /*
     Defensive hardening:
     During scheduler/async flushes there may be no tenant
     context present yet or it may have already been cleared.

     Do not explode inside entity listener.
    */
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