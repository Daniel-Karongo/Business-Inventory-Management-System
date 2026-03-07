package com.IntegrityTechnologies.business_manager.modules.platform.subscription.controller;

import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.PlatformAdminOnly;
import com.IntegrityTechnologies.business_manager.modules.platform.subscription.entity.Plan;
import com.IntegrityTechnologies.business_manager.modules.platform.subscription.repository.PlanRepository;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@Tag(name = "Subscription Plans")
@RestController
@RequestMapping("/api/platform/plans")
@RequiredArgsConstructor
@PlatformAdminOnly
public class SubscriptionController {

    private final PlanRepository repository;

    @GetMapping
    public List<Plan> getPlans() {

        return repository.findAll();

    }

}