package com.IntegrityTechnologies.business_manager.modules.platform.tenant.service;

import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.entity.Tenant;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.entity.TenantStatus;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.repository.TenantRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;

import java.util.UUID;
import java.util.function.Consumer;

@Slf4j
@Service
@RequiredArgsConstructor
public class TenantExecutionService {

    private static final int TENANT_BATCH_SIZE = 100;

    private final TenantRepository tenantRepository;

    /**
     * Execute a task once for every active tenant.
     * Uses paging to avoid loading all tenants into memory.
     */
    public void forEachTenant(Consumer<UUID> task) {

        int page = 0;
        Page<Tenant> tenants;

        do {

            tenants = tenantRepository.findByStatus(
                    TenantStatus.ACTIVE,
                    PageRequest.of(page++, TENANT_BATCH_SIZE)
            );

            for (Tenant tenant : tenants.getContent()) {

                UUID tenantId = tenant.getId();

                try {

                    TenantContext.setTenantId(tenantId);

                    log.debug("Executing task for tenant {}", tenantId);

                    task.accept(tenantId);

                } catch (Exception e) {

                    log.error(
                            "Tenant job failed for tenant {}",
                            tenantId,
                            e
                    );

                } finally {

                    TenantContext.clear();

                }
            }

        } while (tenants.hasNext());
    }
}