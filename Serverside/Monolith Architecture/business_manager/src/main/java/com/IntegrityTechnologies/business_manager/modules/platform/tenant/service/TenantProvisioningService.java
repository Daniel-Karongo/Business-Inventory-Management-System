package com.IntegrityTechnologies.business_manager.modules.platform.tenant.service;

import com.IntegrityTechnologies.business_manager.modules.platform.audit.entity.AuditEntityType;
import com.IntegrityTechnologies.business_manager.modules.platform.audit.service.AuditService;
import com.IntegrityTechnologies.business_manager.modules.platform.subscription.entity.Plan;
import com.IntegrityTechnologies.business_manager.modules.platform.subscription.entity.SubscriptionStatus;
import com.IntegrityTechnologies.business_manager.modules.platform.subscription.entity.TenantSubscription;
import com.IntegrityTechnologies.business_manager.modules.platform.subscription.repository.PlanRepository;
import com.IntegrityTechnologies.business_manager.modules.platform.subscription.repository.TenantSubscriptionRepository;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.entity.Tenant;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.entity.TenantStatus;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.repository.TenantRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.service.TenantInventorySettingsService;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.time.LocalDate;

@Service
@RequiredArgsConstructor
public class TenantProvisioningService {

    private final TenantRepository tenantRepository;
    private final PlanRepository planRepository;
    private final TenantSubscriptionRepository subscriptionRepository;
    private final AuditService auditService;
    private final TenantInventorySettingsService tenantInventorySettingsService;

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

        auditService.log(
                AuditEntityType.TENANT,
                tenant.getId(),
                "CREATE",
                null,
                code
        );

        Plan plan = planRepository.findByCode("FREE")
                .orElseThrow(() ->
                        new IllegalStateException(
                                "Default plan 'FREE' missing. Seed subscription plans first."
                        ));

        TenantSubscription subscription = TenantSubscription.builder()
                .tenantId(tenant.getId())
                .plan(plan)
                .status(SubscriptionStatus.ACTIVE)
                .startDate(LocalDate.now())
                .build();

        subscriptionRepository.save(subscription);

        tenantInventorySettingsService.initialize(tenant.getId());

        return tenant;
    }

}