package com.IntegrityTechnologies.business_manager.modules.platform.tenant.service;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.entity.Tenant;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.entity.TenantStatus;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.repository.TenantRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class TenantServiceImpl implements TenantService {

    private final TenantRepository tenantRepository;

    @Override
    public Tenant createTenant(String name, String code) {

        if (tenantRepository.existsByCode(code)) {
            throw new RuntimeException("Tenant code already exists");
        }

        Tenant tenant = Tenant.builder()
                .name(name)
                .code(code)
                .status(TenantStatus.ACTIVE)
                .platformTenant(false)
                .build();

        return tenantRepository.save(tenant);
    }

    @Override
    public Tenant getTenant(UUID id) {
        return tenantRepository.findById(id)
                .orElseThrow(() -> new RuntimeException("Tenant not found"));
    }

    @Override
    public List<Tenant> getAllTenants() {
        return tenantRepository.findAll();
    }

    @Override
    public void deactivateTenant(UUID id) {

        Tenant tenant = getTenant(id);

        tenant.setStatus(TenantStatus.SUSPENDED);

        tenantRepository.save(tenant);
    }
}