package com.IntegrityTechnologies.business_manager.modules.platform.tenant.service;

import com.IntegrityTechnologies.business_manager.modules.platform.subscription.entity.Plan;
import com.IntegrityTechnologies.business_manager.modules.platform.subscription.entity.TenantSubscription;
import com.IntegrityTechnologies.business_manager.modules.platform.subscription.entity.SubscriptionStatus;
import com.IntegrityTechnologies.business_manager.modules.platform.subscription.repository.PlanRepository;
import com.IntegrityTechnologies.business_manager.modules.platform.subscription.repository.TenantSubscriptionRepository;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.entity.Tenant;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.entity.TenantStatus;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.repository.TenantRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.UUID;

@Service
@RequiredArgsConstructor
public class TenantAdminService {

    private final TenantRepository tenantRepository;
    private final TenantSubscriptionRepository subscriptionRepository;
    private final PlanRepository planRepository;

    public void suspendTenant(UUID tenantId) {

        Tenant tenant = tenantRepository.findById(tenantId)
                .orElseThrow();

        tenant.setStatus(TenantStatus.SUSPENDED);

        tenantRepository.save(tenant);
    }

    public void activateTenant(UUID tenantId) {

        Tenant tenant = tenantRepository.findById(tenantId)
                .orElseThrow();

        tenant.setStatus(TenantStatus.ACTIVE);

        tenantRepository.save(tenant);
    }

    public void changePlan(UUID tenantId, String planCode) {

        TenantSubscription subscription =
                subscriptionRepository.findByTenantId(tenantId)
                        .orElseThrow();

        Plan plan = planRepository.findByCode(planCode)
                .orElseThrow();

        subscription.setPlan(plan);

        subscriptionRepository.save(subscription);
    }

    public void expireTenant(UUID tenantId) {

        Tenant tenant = tenantRepository.findById(tenantId)
                .orElseThrow();

        tenant.setStatus(TenantStatus.EXPIRED);

        tenantRepository.save(tenant);
    }

}