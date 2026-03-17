package com.IntegrityTechnologies.business_manager.modules.platform.tenant.service;

import com.IntegrityTechnologies.business_manager.modules.platform.audit.entity.AuditEntityType;
import com.IntegrityTechnologies.business_manager.modules.platform.audit.service.AuditService;
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

        return TenantMapper.toDto(tenant);
    }

    /* ==========================================
       GET SINGLE TENANT
    ========================================== */

    @Override
    public TenantResponse getTenant(UUID id) {

        Tenant tenant = tenantRepository.findById(id)
                .orElseThrow(() -> new RuntimeException("Tenant not found"));

        return TenantMapper.toDto(tenant);
    }

    /* ==========================================
       PAGINATED TENANTS
    ========================================== */

    @Override
    public Page<TenantResponse> getTenants(int page, int size) {

        return tenantRepository
                .findAll(PageRequest.of(page, size))
                .map(TenantMapper::toDto);

    }

    /* ==========================================
       DEACTIVATE TENANT
    ========================================== */

    @Override
    public void deactivateTenant(UUID id) {

        Tenant tenant = tenantRepository.findById(id)
                .orElseThrow(() -> new RuntimeException("Tenant not found"));

        tenant.setStatus(TenantStatus.SUSPENDED);

        tenantRepository.save(tenant);

        auditService.log(
                AuditEntityType.TENANT,
                tenant.getId(),
                "DEACTIVATE",
                tenant.getStatus().name(),
                TenantStatus.SUSPENDED.name()
        );
    }

}