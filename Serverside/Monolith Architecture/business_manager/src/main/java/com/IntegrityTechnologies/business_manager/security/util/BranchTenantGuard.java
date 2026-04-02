package com.IntegrityTechnologies.business_manager.security.util;

import com.IntegrityTechnologies.business_manager.modules.person.branch.repository.BranchRepository;
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