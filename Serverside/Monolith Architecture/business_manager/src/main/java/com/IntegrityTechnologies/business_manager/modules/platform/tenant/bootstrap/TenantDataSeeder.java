package com.IntegrityTechnologies.business_manager.modules.platform.tenant.bootstrap;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.entity.Tenant;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.entity.TenantStatus;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.repository.TenantRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.boot.CommandLineRunner;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class TenantDataSeeder implements CommandLineRunner {

    private final TenantRepository tenantRepository;

    @Override
    public void run(String... args) {

        if (tenantRepository.count() == 0) {

            Tenant systemTenant = Tenant.builder()
                    .name("Default Tenant")
                    .code("default")
                    .status(TenantStatus.ACTIVE)
                    .platformTenant(false)
                    .build();

            tenantRepository.save(systemTenant);
        }
    }
}