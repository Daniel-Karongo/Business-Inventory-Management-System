package com.IntegrityTechnologies.business_manager.modules.platform.subscription.service;

import com.IntegrityTechnologies.business_manager.modules.platform.subscription.dto.TenantSubscriptionResponse;
import com.IntegrityTechnologies.business_manager.modules.platform.subscription.entity.TenantSubscription;
import com.IntegrityTechnologies.business_manager.modules.platform.subscription.repository.TenantSubscriptionRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.UUID;

@Service
@RequiredArgsConstructor
public class TenantSubscriptionQueryService {

    private final TenantSubscriptionRepository repository;

    public TenantSubscriptionResponse getSubscription(UUID tenantId) {

        TenantSubscription subscription =
                repository.findByTenantId(tenantId)
                        .orElseThrow();

        var plan = subscription.getPlan();

        return TenantSubscriptionResponse.builder()
                .tenantId(tenantId)
                .planCode(plan.getCode())
                .planName(plan.getName())
                .maxUsers(plan.getMaxUsers())
                .maxBranches(plan.getMaxBranches())
                .inventoryEnabled(plan.isInventoryEnabled())
                .accountingEnabled(plan.isAccountingEnabled())
                .reportingEnabled(plan.isReportingEnabled())
                .startDate(subscription.getStartDate())
                .build();

    }

}