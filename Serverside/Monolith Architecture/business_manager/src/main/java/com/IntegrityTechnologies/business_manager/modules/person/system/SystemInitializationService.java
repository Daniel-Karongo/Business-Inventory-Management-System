package com.IntegrityTechnologies.business_manager.modules.person.system;

import com.IntegrityTechnologies.business_manager.modules.person.branch.model.Branch;
import com.IntegrityTechnologies.business_manager.modules.person.branch.repository.BranchRepository;
import com.IntegrityTechnologies.business_manager.modules.platform.identity.entity.PlatformRole;
import com.IntegrityTechnologies.business_manager.modules.platform.identity.entity.PlatformUser;
import com.IntegrityTechnologies.business_manager.modules.platform.identity.repository.PlatformUserRepository;
import com.IntegrityTechnologies.business_manager.modules.platform.subscription.entity.TenantSubscription;
import com.IntegrityTechnologies.business_manager.modules.platform.subscription.repository.PlanRepository;
import com.IntegrityTechnologies.business_manager.modules.platform.subscription.repository.TenantSubscriptionRepository;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.bootstrap.TenantBootstrapService;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.entity.Tenant;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.repository.TenantRepository;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.service.TenantProvisioningService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.UUID;

@Service
@RequiredArgsConstructor
@Slf4j
public class SystemInitializationService {

    private final TenantRepository tenantRepository;
    private final TenantProvisioningService tenantProvisioningService;
    private final TenantBootstrapService tenantBootstrapService;
    private final PlatformUserRepository platformUserRepository;
    private final PasswordEncoder passwordEncoder;
    private final BranchRepository branchRepository;
    private final PlanRepository planRepository;
    private final TenantSubscriptionRepository tenantSubscriptionRepository;

    @Transactional
    public void initialize() {
        /* =====================================
           PLATFORM ROOT ACCOUNT
        ===================================== */

        platformUserRepository
                .findByUsernameAndDeletedFalse("root")
                .orElseGet(() -> {

                    String password = UUID.randomUUID()
                            .toString()
                            .replace("-", "")
                            .substring(0, 16);

                    PlatformUser root = PlatformUser.builder()
                            .username("root")
                            .password(passwordEncoder.encode(password))
                            .role(PlatformRole.PLATFORM_SUPER_ADMIN)
                            .mustChangePassword(true)
                            .active(true)
                            .locked(false)
                            .deleted(false)
                            .build();

                    platformUserRepository.save(root);

                    log.warn("""
                        ---------------------------------------------------------
                        🔐 PLATFORM ROOT ACCOUNT CREATED
                        
                        Username: root
                        Password: {}
                        
                        ⚠️ Change this password immediately
                        ---------------------------------------------------------
                        """, password);

                    return root;
                });

        /* =====================================
           TENANT INITIALIZATION
        ===================================== */

        Tenant platformTenant = tenantRepository
                .findByCodeIgnoreCase("platform")
                .orElseGet(() -> {

                    log.warn("⚠️ Creating platform tenant");

                    Tenant t = Tenant.builder()
                            .name("Platform")
                            .code("platform")
                            .platformTenant(true)
                            .build();

                    return tenantRepository.save(t);
                });

        Tenant tenant = tenantRepository
                .findByCodeIgnoreCase("default")
                .orElseGet(() -> {

                    log.warn("⚠️ No tenants found. Creating default tenant.");

                    return tenantProvisioningService.provisionTenant(
                            "Default Tenant",
                            "default"
                    );

                });

        planRepository.findByCode("FREE").ifPresent(plan -> {

            boolean exists = tenantSubscriptionRepository
                    .existsByTenantId(tenant.getId());

            if (!exists) {

                tenantSubscriptionRepository.save(
                        TenantSubscription.builder()
                                .tenantId(tenant.getId())
                                .plan(plan)
                                .build()
                );

                log.warn("✅ Default subscription assigned (FREE) to tenant {}", tenant.getCode());
            }
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

                UUID branchId = branchRepository
                        .findByTenantIdAndBranchCodeIgnoreCaseAndDeletedFalse(tenant.getId(), "MAIN")
                        .map(Branch::getId)
                        .orElse(null);

                log.warn("""
                    ---------------------------------------------------------
                    🔐 BOOTSTRAP ADMIN ACCOUNT CREATED
                    
                    tenantId: {}
                    tenant code: {}
                    Branch: {}
                    Username: admin
                    Password: {}
                    
                    ⚠️ Change this password immediately
                    ---------------------------------------------------------
                    """,
                        tenant.getId(),
                        tenant.getCode(),
                        branchId,
                        password
                );

            } else {

                log.info("Admin user already exists — bootstrap verified.");

            }

        } finally {
            TenantContext.clear();
        }

        log.info("🎉 System initialization completed.");
    }
}