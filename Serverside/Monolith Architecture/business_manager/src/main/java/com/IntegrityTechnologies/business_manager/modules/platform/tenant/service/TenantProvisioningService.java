package com.IntegrityTechnologies.business_manager.modules.platform.tenant.service;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.entity.*;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.repository.TenantRepository;
import com.IntegrityTechnologies.business_manager.modules.platform.subscription.entity.*;
import com.IntegrityTechnologies.business_manager.modules.platform.subscription.repository.*;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.time.LocalDate;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class TenantProvisioningService {

    private final TenantRepository tenantRepository;
    private final PlanRepository planRepository;
    private final TenantSubscriptionRepository subscriptionRepository;

    public Tenant provisionTenant(String name, String code) {

        if (tenantRepository.existsByCode(code)) {
            throw new IllegalStateException("Tenant already exists");
        }

        Tenant tenant = Tenant.builder()
                .name(name)
                .code(code)
                .status(TenantStatus.ACTIVE)
                .platformTenant(false)
                .build();

        tenantRepository.save(tenant);

        Plan plan = planRepository.findByCode("FREE")
                .orElseThrow(() -> new IllegalStateException("Default plan missing"));

        TenantSubscription subscription = TenantSubscription.builder()
                .tenantId(tenant.getId())
                .plan(plan)
                .status(SubscriptionStatus.ACTIVE)
                .startDate(LocalDate.now())
                .build();

        subscriptionRepository.save(subscription);

        return tenant;
    }

}