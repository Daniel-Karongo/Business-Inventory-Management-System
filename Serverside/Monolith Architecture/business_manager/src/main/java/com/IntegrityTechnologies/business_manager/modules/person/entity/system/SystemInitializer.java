package com.IntegrityTechnologies.business_manager.modules.person.entity.system;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.bootstrap.TenantBootstrapService;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.context.TenantContext;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.entity.Tenant;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.repository.TenantRepository;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.service.TenantProvisioningService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.ApplicationRunner;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.util.UUID;

@Configuration
@RequiredArgsConstructor
@Slf4j
public class SystemInitializer {

    private final TenantRepository tenantRepository;
    private final TenantProvisioningService tenantProvisioningService;
    private final TenantBootstrapService tenantBootstrapService;

    @Bean
    public ApplicationRunner initializeDefaults() {

        return args -> {

            Tenant tenant = tenantRepository
                    .findByCode("default")
                    .orElseGet(() -> {

                        log.warn("⚠️ No tenants found. Creating default tenant.");

                        return tenantProvisioningService.provisionTenant(
                                "Default Tenant",
                                "default"
                        );

                    });

            log.info("Ensuring tenant bootstrap...");

            String password = UUID.randomUUID()
                    .toString()
                    .replace("-", "")
                    .substring(0, 16);

            TenantContext.setTenantId(tenant.getId());

            try {

                boolean adminCreated = tenantBootstrapService.bootstrapTenant(
                        tenant.getId(),
                        "admin",
                        password
                );

                if (adminCreated) {

                    log.warn("""
                ---------------------------------------------------------
                🔐 BOOTSTRAP ADMIN ACCOUNT CREATED
        
                Username: admin
                Password: {}
        
                ⚠️ Change this password immediately
                ---------------------------------------------------------
                """, password);

                } else {

                    log.info("Admin user already exists — bootstrap verified.");

                }

            } finally {
                TenantContext.clear();
            }

            log.info("🎉 System initialization completed.");
        };
    }
}