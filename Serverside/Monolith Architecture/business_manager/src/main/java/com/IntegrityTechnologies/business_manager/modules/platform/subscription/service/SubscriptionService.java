package com.IntegrityTechnologies.business_manager.modules.platform.subscription.service;

import com.IntegrityTechnologies.business_manager.modules.platform.subscription.entity.Plan;
import com.IntegrityTechnologies.business_manager.modules.platform.subscription.entity.TenantSubscription;
import com.IntegrityTechnologies.business_manager.modules.platform.subscription.repository.TenantSubscriptionRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.UUID;

@Service
@RequiredArgsConstructor
public class SubscriptionService {

    private final TenantSubscriptionRepository subscriptionRepository;

    public Plan getPlan(UUID tenantId) {

        TenantSubscription subscription =
                subscriptionRepository
                        .findByTenantId(tenantId)
                        .orElseThrow(() ->
                                new IllegalStateException("Tenant has no subscription"));

        return subscription.getPlan();
    }

}