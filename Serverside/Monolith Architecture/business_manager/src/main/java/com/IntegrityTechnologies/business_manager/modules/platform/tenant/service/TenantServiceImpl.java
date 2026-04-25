package com.IntegrityTechnologies.business_manager.modules.platform.tenant.service;

import com.IntegrityTechnologies.business_manager.modules.platform.audit.entity.AuditEntityType;
import com.IntegrityTechnologies.business_manager.modules.platform.audit.service.AuditService;
import com.IntegrityTechnologies.business_manager.modules.platform.subscription.entity.Plan;
import com.IntegrityTechnologies.business_manager.modules.platform.subscription.entity.SubscriptionStatus;
import com.IntegrityTechnologies.business_manager.modules.platform.subscription.entity.TenantSubscription;
import com.IntegrityTechnologies.business_manager.modules.platform.subscription.repository.PlanRepository;
import com.IntegrityTechnologies.business_manager.modules.platform.subscription.repository.TenantSubscriptionRepository;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.bootstrap.TenantBootstrapService;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.dto.TenantCreateRequest;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.dto.TenantResponse;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.entity.Tenant;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.entity.TenantStatus;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.mapper.TenantMapper;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.repository.TenantRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;

import java.time.LocalDate;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class TenantServiceImpl implements TenantService {

    private final TenantRepository tenantRepository;
    private final TenantBootstrapService tenantBootstrapService;
    private final AuditService auditService;
    private final TenantProvisioningService tenantProvisioningService;
    private final TenantSubscriptionRepository subscriptionRepository;
    private final PlanRepository planRepository;


    /* ==========================================
       CREATE TENANT
    ========================================== */

    @Override
    @CacheEvict(value = "tenants", allEntries = true)
    public TenantResponse createTenant(TenantCreateRequest request) {

        if (tenantRepository.existsByCode(request.getCode())) {
            throw new RuntimeException("Tenant code already exists");
        }

        Tenant tenant = tenantProvisioningService.provisionTenant(
                request.getName(),
                request.getCode()
        );

        tenantBootstrapService.bootstrapTenant(
                tenant.getId(),
                request.getAdminUsername() != null ? request.getAdminUsername() : "admin",
                request.getAdminPassword() != null ? request.getAdminPassword() : "ChangeMe123!"
        );

        var sub =
                subscriptionRepository
                        .findByTenantId(
                                tenant.getId()
                        )
                        .orElseThrow();

        return TenantMapper.toDto(
                tenant,
                sub.getPlan().getCode(),
                sub.getStatus().name()
        );
    }

    /* ==========================================
       GET SINGLE TENANT
    ========================================== */

    @Override
    public TenantResponse getTenant(UUID id) {

        Tenant tenant = tenantRepository.findById(id)
                .orElseThrow(() -> new RuntimeException("Tenant not found"));

        var sub =
                subscriptionRepository.findByTenantId(
                        tenant.getId()
                ).orElseGet(() -> {

                    Plan freePlan =
                            planRepository
                                    .findByCode("FREE")
                                    .orElseThrow();

                    return subscriptionRepository.save(
                            TenantSubscription.builder()
                                    .tenantId(tenant.getId())
                                    .plan(freePlan)
                                    .status(
                                            SubscriptionStatus.ACTIVE
                                    )
                                    .startDate(LocalDate.now())
                                    .build()
                    );

                });

        return TenantMapper.toDto(
                tenant,
                sub.getPlan().getCode(),
                sub.getStatus().name()
        );
    }

    /* ==========================================
       PAGINATED TENANTS
    ========================================== */

    @Override
    public Page<TenantResponse> getTenants(
            int page,
            int size,
            String search
    ) {

        Page<Tenant> tenants =
                tenantRepository.searchTenants(
                        search,
                        PageRequest.of(page, size)
                );

        var ids =
                tenants.getContent()
                        .stream()
                        .map(Tenant::getId)
                        .toList();

        var subscriptions =
                subscriptionRepository.findAllByTenantIds(ids);

        var byTenant =
                subscriptions.stream()
                        .collect(
                                java.util.stream.Collectors.toMap(
                                        s -> s.getTenantId(),
                                        s -> s
                                )
                        );

        return tenants.map(t -> {

            var sub =
                    byTenant.get(t.getId());

            if (sub == null) {

                Plan freePlan =
                        planRepository
                                .findByCode("FREE")
                                .orElseThrow();

                sub =
                        subscriptionRepository.save(
                                TenantSubscription.builder()
                                        .tenantId(t.getId())
                                        .plan(freePlan)
                                        .status(
                                                SubscriptionStatus.ACTIVE
                                        )
                                        .startDate(
                                                LocalDate.now()
                                        )
                                        .build()
                        );
            }

            return TenantMapper.toDto(
                    t,
                    sub.getPlan().getCode(),
                    sub.getStatus().name()
            );

        });
    }
}