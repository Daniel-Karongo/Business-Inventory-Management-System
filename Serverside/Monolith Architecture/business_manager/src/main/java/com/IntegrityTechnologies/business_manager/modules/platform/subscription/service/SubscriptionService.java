package com.IntegrityTechnologies.business_manager.modules.platform.subscription.service;

import com.IntegrityTechnologies.business_manager.modules.platform.subscription.entity.Plan;
import com.IntegrityTechnologies.business_manager.modules.platform.subscription.entity.TenantSubscription;
import com.IntegrityTechnologies.business_manager.modules.platform.subscription.repository.PlanRepository;
import com.IntegrityTechnologies.business_manager.modules.platform.subscription.repository.TenantSubscriptionRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.UUID;

@Service
@RequiredArgsConstructor
public class SubscriptionService {

    private final PlanRepository planRepository;

    public Plan getPlan(UUID tenantId) {

        return planRepository
                .findPlanByTenantId(tenantId)
                .orElse(null); // ⚠️ allow null for platform
    }
}