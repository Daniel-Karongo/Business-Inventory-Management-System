package com.IntegrityTechnologies.business_manager.modules.platform.subscription.bootstrap;

import com.IntegrityTechnologies.business_manager.modules.platform.subscription.entity.Plan;
import com.IntegrityTechnologies.business_manager.modules.platform.subscription.repository.PlanRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

@Component
@RequiredArgsConstructor
public class PlanBootstrapService {

    private final PlanRepository repository;

    @Transactional
    public void bootstrap() {

        upsert(
                "FREE",
                "Free Plan",
                15,
                2,
                true,
                false,
                false,
                300
        );

        upsert(
                "PRO",
                "Professional",
                75,
                10,
                true,
                true,
                true,
                1500
        );

        upsert(
                "ENTERPRISE",
                "Enterprise",
                1000,
                100,
                true,
                true,
                true,
                10000
        );
    }

    private void upsert(
            String code,
            String name,
            int maxUsers,
            int maxBranches,
            boolean inventory,
            boolean accounting,
            boolean reporting,
            int rpm
    ) {

        Plan plan =
                repository.findByCode(code)
                        .orElseGet(Plan::new);

        plan.setCode(code);
        plan.setName(name);
        plan.setMaxUsers(maxUsers);
        plan.setMaxBranches(maxBranches);
        plan.setInventoryEnabled(inventory);
        plan.setAccountingEnabled(accounting);
        plan.setReportingEnabled(reporting);
        plan.setRequestsPerMinute(rpm);

        repository.save(plan);
    }
}