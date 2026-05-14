package com.IntegrityTechnologies.business_manager.modules.platform.tenant.service;

import com.IntegrityTechnologies.business_manager.modules.platform.subscription.entity.Plan;
import com.IntegrityTechnologies.business_manager.modules.platform.subscription.entity.TenantSubscription;
import com.IntegrityTechnologies.business_manager.modules.platform.subscription.repository.PlanRepository;
import com.IntegrityTechnologies.business_manager.modules.platform.subscription.repository.TenantSubscriptionRepository;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.entity.Tenant;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.entity.TenantStatus;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.repository.TenantRepository;
import com.IntegrityTechnologies.business_manager.security.cache.TenantMetadataCache;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.transaction.support.TransactionSynchronization;
import org.springframework.transaction.support.TransactionSynchronizationManager;

import java.util.UUID;

@Service
@Transactional
@RequiredArgsConstructor
public class TenantAdminService {

    private final TenantRepository tenantRepository;
    private final TenantSubscriptionRepository subscriptionRepository;
    private final PlanRepository planRepository;
    private final TenantMetadataCache tenantMetadataCache;

    public void suspendTenant(UUID tenantId) {

        Tenant tenant =
                tenantRepository.findById(tenantId)
                        .orElseThrow();

        tenant.setStatus(TenantStatus.SUSPENDED);

        tenantRepository.save(tenant);

        afterCommit(() ->
                tenantMetadataCache.invalidateTenantStatus(tenantId)
        );
    }

    public void activateTenant(UUID tenantId) {

        Tenant tenant =
                tenantRepository.findById(tenantId)
                        .orElseThrow();

        tenant.setStatus(TenantStatus.ACTIVE);

        tenantRepository.save(tenant);

        afterCommit(() ->
                tenantMetadataCache.invalidateTenantStatus(tenantId)
        );
    }

    public void expireTenant(UUID tenantId) {

        Tenant tenant =
                tenantRepository.findById(tenantId)
                        .orElseThrow();

        tenant.setStatus(TenantStatus.EXPIRED);

        tenantRepository.save(tenant);

        afterCommit(() ->
                tenantMetadataCache.invalidateTenantStatus(tenantId)
        );
    }

    public void changePlan(
            UUID tenantId,
            String planCode
    ) {

        TenantSubscription subscription =
                subscriptionRepository.findByTenantId(tenantId)
                        .orElseThrow();

        Plan plan =
                planRepository.findByCode(planCode)
                        .orElseThrow();

        subscription.setPlan(plan);

        subscriptionRepository.save(subscription);

        afterCommit(() ->
                tenantMetadataCache.invalidateSubscription(tenantId)
        );
    }

    private void afterCommit(Runnable runnable) {

        TransactionSynchronizationManager
                .registerSynchronization(
                        new TransactionSynchronization() {
                            @Override
                            public void afterCommit() {
                                runnable.run();
                            }
                        }
                );
    }
}