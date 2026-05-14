package com.IntegrityTechnologies.business_manager.modules.platform.subscription.service;

import com.IntegrityTechnologies.business_manager.modules.platform.subscription.entity.Plan;
import com.IntegrityTechnologies.business_manager.modules.platform.subscription.repository.PlanRepository;
import com.IntegrityTechnologies.business_manager.modules.platform.subscription.repository.TenantSubscriptionRepository;
import com.IntegrityTechnologies.business_manager.security.cache.TenantMetadataCache;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.UUID;

@Service
@Transactional
@RequiredArgsConstructor
public class PlanService {

    private final PlanRepository repository;
    private final TenantSubscriptionRepository subscriptionRepository;
    private final TenantMetadataCache tenantMetadataCache;

    public Plan createPlan(Plan plan) {

        if (repository.findByCode(plan.getCode()).isPresent()) {
            throw new IllegalStateException(
                    "Plan already exists: " + plan.getCode()
            );
        }

        return repository.save(plan);
    }

    public Plan updatePlan(
            UUID id,
            Plan updated
    ) {

        Plan existing =
                repository.findById(id)
                        .orElseThrow(() ->
                                new IllegalStateException(
                                        "Plan not found"
                                ));

        existing.setName(updated.getName());
        existing.setMaxUsers(updated.getMaxUsers());
        existing.setMaxBranches(updated.getMaxBranches());
        existing.setInventoryEnabled(updated.isInventoryEnabled());
        existing.setAccountingEnabled(updated.isAccountingEnabled());
        existing.setReportingEnabled(updated.isReportingEnabled());
        existing.setRequestsPerMinute(updated.getRequestsPerMinute());

        Plan saved =
                repository.save(existing);

        subscriptionRepository.findAll()
                .stream()
                .filter(s ->
                        s.getPlan() != null &&
                                s.getPlan().getId().equals(saved.getId())
                )
                .map(s -> s.getTenantId())
                .forEach(tenantMetadataCache::invalidateSubscription);

        return saved;
    }

    public void deletePlan(UUID id) {
        repository.deleteById(id);
    }

    public List<Plan> getAll() {
        return repository.findAll();
    }
}