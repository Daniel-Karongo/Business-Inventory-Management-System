package com.IntegrityTechnologies.business_manager.security.cache;

import com.IntegrityTechnologies.business_manager.modules.person.branch.repository.BranchRepository;
import com.IntegrityTechnologies.business_manager.modules.platform.subscription.entity.Plan;
import com.IntegrityTechnologies.business_manager.modules.platform.subscription.service.SubscriptionService;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.entity.Tenant;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.entity.TenantStatus;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.repository.TenantRepository;
import lombok.Builder;
import lombok.RequiredArgsConstructor;
import lombok.Value;
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

    /*
     =========================================
     IMMUTABLE SNAPSHOTS
     =========================================
     */

    @Value
    @Builder
    public static class PlanSnapshot {
        UUID id;
        String code;
        String name;
        int maxUsers;
        int maxBranches;
        boolean inventoryEnabled;
        boolean accountingEnabled;
        boolean reportingEnabled;
        int requestsPerMinute;
    }

    /*
     =========================================
     INTERNAL CACHE STRUCTURES
     =========================================
     */

    private final Map<UUID, TenantStatus> tenantStatusCache =
            new ConcurrentHashMap<>();

    private final Map<String, UUID> tenantCodeCache =
            new ConcurrentHashMap<>();

    private final Map<UUID, PlanSnapshot> subscriptionCache =
            new ConcurrentHashMap<>();

    private final Map<UUID, Set<UUID>> tenantBranches =
            new ConcurrentHashMap<>();

    /*
     =========================================
     ACTIVE TENANT CODES
     =========================================
     */

    public Set<String> getActiveTenantCodes() {
        return tenantRepository
                .findByStatusIn(
                        java.util.List.of(
                                TenantStatus.ACTIVE,
                                TenantStatus.TRIAL
                        )
                )
                .stream()
                .map(Tenant::getCode)
                .map(String::toLowerCase)
                .collect(java.util.stream.Collectors.toUnmodifiableSet());
    }

    /*
     =========================================
     TENANT LOOKUP BY CODE
     =========================================
     */

    public UUID getTenantIdByCode(String code) {

        final String normalizedCode =
                normalize(code);

        UUID cached =
                tenantCodeCache.get(normalizedCode);

        if (cached != null) {
            return cached;
        }

        Tenant tenant =
                tenantRepository
                        .findByCodeIgnoreCase(normalizedCode)
                        .orElseThrow(() ->
                                new IllegalStateException(
                                        "Tenant not found: " + normalizedCode
                                ));

        tenantCodeCache.put(
                normalizedCode,
                tenant.getId()
        );

        tenantStatusCache.put(
                tenant.getId(),
                tenant.getStatus()
        );

        return tenant.getId();
    }

    /*
     =========================================
     TENANT STATUS
     =========================================
     */

    public boolean isPlatformTenant(UUID tenantId) {

        if (tenantId == null) {
            return false;
        }

        return tenantRepository.findById(tenantId)
                .map(Tenant::isPlatformTenant)
                .orElse(false);
    }

    public TenantStatus getTenantStatus(UUID tenantId) {

        TenantStatus cached =
                tenantStatusCache.get(tenantId);

        if (cached != null) {
            return cached;
        }

        Tenant tenant =
                tenantRepository.findById(tenantId)
                        .orElseThrow(() ->
                                new IllegalStateException(
                                        "Tenant not found"
                                ));

        tenantStatusCache.put(
                tenantId,
                tenant.getStatus()
        );

        return tenant.getStatus();
    }

    public boolean tenantExists(UUID tenantId) {

        if (tenantId == null) {
            return false;
        }

        return tenantRepository.existsById(tenantId);
    }

    /*
     =========================================
     SUBSCRIPTION PLAN
     =========================================
     */

    public PlanSnapshot getSubscriptionPlan(UUID tenantId) {

        PlanSnapshot cached =
                subscriptionCache.get(tenantId);

        if (cached != null) {
            return cached;
        }

        Plan plan =
                subscriptionService.getPlan(tenantId);

        if (plan == null) {
            return null;
        }

        PlanSnapshot snapshot =
                toSnapshot(plan);

        subscriptionCache.put(
                tenantId,
                snapshot
        );

        return snapshot;
    }

    /*
     =========================================
     BRANCH VALIDATION
     =========================================
     */

    public boolean branchExists(
            UUID tenantId,
            UUID branchId
    ) {

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

    /*
     =========================================
     INVALIDATION
     =========================================
     */

    public void invalidateTenant(UUID tenantId) {

        tenantStatusCache.remove(tenantId);

        subscriptionCache.remove(tenantId);

        tenantBranches.remove(tenantId);

        tenantCodeCache.entrySet()
                .removeIf(e ->
                        e.getValue().equals(tenantId)
                );
    }

    public void invalidateSubscription(UUID tenantId) {
        subscriptionCache.remove(tenantId);
    }

    public void invalidateTenantStatus(UUID tenantId) {
        tenantStatusCache.remove(tenantId);
    }

    public void invalidateTenantCode(UUID tenantId) {
        tenantCodeCache.entrySet()
                .removeIf(e ->
                        e.getValue().equals(tenantId)
                );
    }

    public void invalidateBranchCache(UUID tenantId) {
        tenantBranches.remove(tenantId);
    }

    public void invalidateAll() {

        tenantStatusCache.clear();

        tenantCodeCache.clear();

        subscriptionCache.clear();

        tenantBranches.clear();
    }

    /*
     =========================================
     HELPERS
     =========================================
     */

    private String normalize(String code) {

        if (code == null) {
            throw new IllegalArgumentException(
                    "Tenant code cannot be null"
            );
        }

        return code.trim().toLowerCase();
    }

    private PlanSnapshot toSnapshot(Plan plan) {

        return PlanSnapshot.builder()
                .id(plan.getId())
                .code(plan.getCode())
                .name(plan.getName())
                .maxUsers(plan.getMaxUsers())
                .maxBranches(plan.getMaxBranches())
                .inventoryEnabled(plan.isInventoryEnabled())
                .accountingEnabled(plan.isAccountingEnabled())
                .reportingEnabled(plan.isReportingEnabled())
                .requestsPerMinute(plan.getRequestsPerMinute())
                .build();
    }
}