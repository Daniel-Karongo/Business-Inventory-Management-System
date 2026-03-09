package com.IntegrityTechnologies.business_manager.modules.platform.subscription.service;

import com.IntegrityTechnologies.business_manager.modules.platform.subscription.entity.Plan;
import com.IntegrityTechnologies.business_manager.modules.platform.subscription.repository.PlanRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class PlanService {

    private final PlanRepository repository;

    public Plan createPlan(Plan plan) {

        if (repository.findByCode(plan.getCode()).isPresent()) {
            throw new IllegalStateException("Plan already exists: " + plan.getCode());
        }

        return repository.save(plan);
    }

    public Plan updatePlan(UUID id, Plan updated) {

        Plan existing = repository.findById(id)
                .orElseThrow(() -> new IllegalStateException("Plan not found"));

        existing.setName(updated.getName());
        existing.setMaxUsers(updated.getMaxUsers());
        existing.setMaxBranches(updated.getMaxBranches());
        existing.setInventoryEnabled(updated.isInventoryEnabled());
        existing.setAccountingEnabled(updated.isAccountingEnabled());
        existing.setReportingEnabled(updated.isReportingEnabled());
        existing.setRequestsPerMinute(updated.getRequestsPerMinute());

        return repository.save(existing);
    }

    public void deletePlan(UUID id) {
        repository.deleteById(id);
    }

    public List<Plan> getAll() {
        return repository.findAll();
    }
}