package com.IntegrityTechnologies.business_manager.modules.platform.subscription.service;

import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.repository.BranchRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.repository.UserRepository;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.context.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.UUID;

@Component
@RequiredArgsConstructor
public class SubscriptionGuard {

    private final SubscriptionService subscriptionService;
    private final UserRepository userRepository;
    private final BranchRepository branchRepository;

    public void checkUserLimit() {

        UUID tenantId = TenantContext.getTenantId();

        int currentUsers =
                userRepository.countByTenantId(tenantId);

        int maxUsers =
                subscriptionService.getPlan(tenantId).getMaxUsers();

        if (currentUsers >= maxUsers) {
            throw new IllegalStateException("User limit exceeded for subscription");
        }

    }

    public void checkBranchLimit() {

        UUID tenantId = TenantContext.getTenantId();

        int branches =
                branchRepository.countByTenantId(tenantId);

        int maxBranches =
                subscriptionService.getPlan(tenantId).getMaxBranches();

        if (branches >= maxBranches) {
            throw new IllegalStateException("Branch limit exceeded for subscription");
        }

    }

}