package com.IntegrityTechnologies.business_manager.security.cache;

import com.IntegrityTechnologies.business_manager.modules.person.branch.repository.BranchRepository;
import com.IntegrityTechnologies.business_manager.modules.platform.subscription.entity.Plan;
import com.IntegrityTechnologies.business_manager.modules.platform.subscription.service.SubscriptionService;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.entity.Tenant;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.entity.TenantStatus;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.repository.TenantRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;

@Component
@RequiredArgsConstructor
public class TenantMetadataCache {

    private final TenantRepository tenantRepository;
    private final BranchRepository branchRepository;
    private final SubscriptionService subscriptionService;

    /* =========================================
       INTERNAL CACHE STRUCTURES
    ========================================= */

    private final Map<UUID, TenantStatus> tenantStatusCache =
            new ConcurrentHashMap<>();

    private final Map<String, UUID> tenantCodeCache =
            new ConcurrentHashMap<>();

    private final Map<UUID, Plan> subscriptionCache =
            new ConcurrentHashMap<>();

    private final Map<UUID, Set<UUID>> tenantBranches =
            new ConcurrentHashMap<>();

    public Set<String> getActiveTenantCodes() {

        return tenantRepository
                .findByStatusIn(java.util.List.of(
                        TenantStatus.ACTIVE,
                        TenantStatus.TRIAL
                ))
                .stream()
                .map(Tenant::getCode)
                .map(String::toLowerCase)
                .collect(java.util.stream.Collectors.toSet());
    }

    /* =========================================
       TENANT LOOKUP BY CODE
    ========================================= */

    public UUID getTenantIdByCode(String code) {

        final String normalizedCode = code.toLowerCase();

        UUID cached = tenantCodeCache.get(normalizedCode);

        if (cached != null) {
            return cached;
        }

        Tenant tenant = tenantRepository.findByCodeIgnoreCase(normalizedCode)
                .orElseThrow(() ->
                        new IllegalStateException("Tenant not found: " + normalizedCode));

        tenantCodeCache.put(normalizedCode, tenant.getId());
        tenantStatusCache.put(tenant.getId(), tenant.getStatus());

        return tenant.getId();
    }

    /* =========================================
       TENANT STATUS
    ========================================= */

    public boolean isPlatformTenant(UUID tenantId) {

        if (tenantId == null) return false;

        return tenantRepository.findById(tenantId)
                .map(Tenant::isPlatformTenant)
                .orElse(false);
    }

    public TenantStatus getTenantStatus(UUID tenantId) {

        TenantStatus status = tenantStatusCache.get(tenantId);

        if (status != null) {
            return status;
        }

        Tenant tenant = tenantRepository.findById(tenantId)
                .orElseThrow(() ->
                        new IllegalStateException("Tenant not found"));

        tenantStatusCache.put(tenantId, tenant.getStatus());

        return tenant.getStatus();
    }

    public boolean tenantExists(UUID tenantId) {

        try {
            getTenantStatus(tenantId);
            return true;
        } catch (Exception e) {
            return false;
        }
    }

    /* =========================================
       SUBSCRIPTION PLAN
    ========================================= */

    public Plan getSubscriptionPlan(UUID tenantId) {

        Plan cached = subscriptionCache.get(tenantId);

        if (cached != null) {
            return cached;
        }

        Plan plan = subscriptionService.getPlan(tenantId);

        if (plan != null) {
            subscriptionCache.putIfAbsent(tenantId, plan);
        }

        return plan;
    }

    /* =========================================
       BRANCH VALIDATION
    ========================================= */

    public boolean branchExists(UUID tenantId, UUID branchId) {

        Set<UUID> branches =
                tenantBranches.computeIfAbsent(
                        tenantId,
                        t -> ConcurrentHashMap.newKeySet()
                );

        if (branches.contains(branchId)) {
            return true;
        }

        boolean exists =
                branchRepository.existsByTenantIdAndId(
                        tenantId,
                        branchId
                );

        if (exists) {
            branches.add(branchId);
        }

        return exists;
    }

    /* =========================================
       CACHE INVALIDATION
    ========================================= */

    public void invalidateTenant(UUID tenantId) {

        tenantStatusCache.remove(tenantId);
        subscriptionCache.remove(tenantId);
        tenantBranches.remove(tenantId);

        tenantCodeCache.entrySet()
                .removeIf(e -> e.getValue().equals(tenantId));
    }
}