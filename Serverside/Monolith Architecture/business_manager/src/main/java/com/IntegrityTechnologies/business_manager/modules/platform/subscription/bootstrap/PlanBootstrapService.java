package com.IntegrityTechnologies.business_manager.modules.platform.subscription.bootstrap;

import com.IntegrityTechnologies.business_manager.modules.platform.subscription.entity.Plan;
import com.IntegrityTechnologies.business_manager.modules.platform.subscription.repository.PlanRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class PlanBootstrapService {

    private final PlanRepository repository;

    public void bootstrap() {

        createIfMissing("FREE", Plan.builder()
                .code("FREE")
                .name("Free Plan")
                .maxUsers(5)
                .maxBranches(1)
                .inventoryEnabled(true)
                .accountingEnabled(false)
                .reportingEnabled(false)
                .requestsPerMinute(300)
                .build());

        createIfMissing("PRO", Plan.builder()
                .code("PRO")
                .name("Professional")
                .maxUsers(50)
                .maxBranches(5)
                .inventoryEnabled(true)
                .accountingEnabled(true)
                .reportingEnabled(true)
                .requestsPerMinute(1500)
                .build());

        createIfMissing("ENTERPRISE", Plan.builder()
                .code("ENTERPRISE")
                .name("Enterprise")
                .maxUsers(1000)
                .maxBranches(100)
                .inventoryEnabled(true)
                .accountingEnabled(true)
                .reportingEnabled(true)
                .requestsPerMinute(10000)
                .build());
    }

    private void createIfMissing(String code, Plan plan) {
        repository.findByCode(code).orElseGet(() -> repository.save(plan));
    }
}