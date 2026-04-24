package com.IntegrityTechnologies.business_manager.modules.platform.tenant.service;

import com.IntegrityTechnologies.business_manager.modules.platform.audit.entity.AuditEntityType;
import com.IntegrityTechnologies.business_manager.modules.platform.audit.service.AuditService;
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

import java.util.UUID;

@Service
@RequiredArgsConstructor
public class TenantServiceImpl implements TenantService {

    private final TenantRepository tenantRepository;
    private final TenantBootstrapService tenantBootstrapService;
    private final AuditService auditService;
    private final TenantProvisioningService tenantProvisioningService;
    private final TenantSubscriptionRepository subscriptionRepository;


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
       PAGINATED TENANTS
    ========================================== */

    @Override
    public Page<TenantResponse> getTenants(
            int page,
            int size
    ) {

        Page<Tenant> tenants =
                tenantRepository.findAll(
                        PageRequest.of(page,size)
                );

        var ids =
                tenants.getContent()
                        .stream()
                        .map(Tenant::getId)
                        .toList();

        var subscriptions =
                subscriptionRepository
                        .findAllByTenantIds(ids);

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
                    byTenant.get(
                            t.getId()
                    );

            return TenantMapper.toDto(
                    t,
                    sub.getPlan().getCode(),
                    sub.getStatus().name()
            );

        });
    }
}