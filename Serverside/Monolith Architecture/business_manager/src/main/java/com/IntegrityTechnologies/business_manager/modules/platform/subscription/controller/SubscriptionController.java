package com.IntegrityTechnologies.business_manager.modules.platform.subscription.controller;

import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.PlatformAdminOnly;
import com.IntegrityTechnologies.business_manager.modules.platform.subscription.entity.Plan;
import com.IntegrityTechnologies.business_manager.modules.platform.subscription.service.PlanService;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.UUID;

@Tag(name = "Subscription Plans")
@RestController
@RequestMapping("/api/platform/plans")
@RequiredArgsConstructor
@PlatformAdminOnly
public class SubscriptionController {

    private final PlanService service;

    @GetMapping
    public List<Plan> getPlans() {
        return service.getAll();
    }

    @PostMapping
    public Plan createPlan(@RequestBody Plan plan) {
        return service.createPlan(plan);
    }

    @PutMapping("/{id}")
    public Plan updatePlan(
            @PathVariable UUID id,
            @RequestBody Plan plan
    ) {
        return service.updatePlan(id, plan);
    }

    @DeleteMapping("/{id}")
    public void deletePlan(@PathVariable UUID id) {
        service.deletePlan(id);
    }
}