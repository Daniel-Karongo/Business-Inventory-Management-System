package com.IntegrityTechnologies.business_manager.security;

import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.repository.BranchRepository;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.context.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.UUID;

@Component
@RequiredArgsConstructor
public class BranchTenantGuard {

    private final BranchRepository branchRepository;

    public void validate(UUID branchId) {

        if (branchId == null) {
            return;
        }

        UUID tenantId = TenantContext.getTenantId();

        boolean valid =
                branchRepository.existsByTenantIdAndId(tenantId, branchId);

        if (!valid) {
            throw new SecurityException(
                    "Branch does not belong to current tenant"
            );
        }
    }
}