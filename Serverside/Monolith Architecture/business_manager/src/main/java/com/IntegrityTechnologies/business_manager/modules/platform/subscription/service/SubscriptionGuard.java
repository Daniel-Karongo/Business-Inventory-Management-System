package com.IntegrityTechnologies.business_manager.modules.platform.subscription.service;

import com.IntegrityTechnologies.business_manager.modules.person.branch.repository.BranchRepository;
import com.IntegrityTechnologies.business_manager.modules.person.user.repository.UserRepository;
import com.IntegrityTechnologies.business_manager.security.cache.TenantMetadataCache;
import com.IntegrityTechnologies.business_manager.security.util.SecurityUtils;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.UUID;

@Component
@RequiredArgsConstructor
public class SubscriptionGuard {

    private final UserRepository userRepository;
    private final BranchRepository branchRepository;
    private final TenantMetadataCache tenantMetadataCache;

    public void checkUserLimit() {

        if (SecurityUtils.isPlatformAdmin()) {
            return;
        }

        UUID tenantId =
                TenantContext.getTenantId();

        int currentUsers =
                userRepository.countByTenantId(tenantId);

        var plan =
                tenantMetadataCache.getSubscriptionPlan(tenantId);

        if (plan == null) {
            return;
        }

        if (currentUsers >= plan.getMaxUsers()) {
            throw new IllegalStateException(
                    "User limit exceeded for subscription"
            );
        }
    }

    public void checkBranchLimit() {

        if (SecurityUtils.isPlatformAdmin()) {
            return;
        }

        UUID tenantId =
                TenantContext.getTenantId();

        int branches =
                branchRepository.countByTenantId(tenantId);

        var plan =
                tenantMetadataCache.getSubscriptionPlan(tenantId);

        if (plan == null) {
            return;
        }

        if (branches >= plan.getMaxBranches()) {
            throw new IllegalStateException(
                    "Branch limit exceeded for subscription"
            );
        }
    }
}