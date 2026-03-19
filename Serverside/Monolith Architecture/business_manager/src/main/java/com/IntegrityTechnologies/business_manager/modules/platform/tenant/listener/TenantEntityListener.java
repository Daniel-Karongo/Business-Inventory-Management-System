package com.IntegrityTechnologies.business_manager.modules.platform.tenant.listener;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.model.BranchAwareEntity;
import com.IntegrityTechnologies.business_manager.security.util.BranchContext;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
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

        UUID current = TenantContext.getTenantId();

        if (!current.equals(tenantAware.getTenantId())) {
            throw new SecurityException("Cross-tenant modification attempt");
        }
    }
}