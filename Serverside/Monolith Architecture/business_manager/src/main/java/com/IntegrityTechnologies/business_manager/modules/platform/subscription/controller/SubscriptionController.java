package com.IntegrityTechnologies.business_manager.modules.platform.subscription.controller;

import com.IntegrityTechnologies.business_manager.modules.platform.subscription.entity.Plan;
import com.IntegrityTechnologies.business_manager.modules.platform.subscription.repository.PlanRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping("/api/platform/plans")
@RequiredArgsConstructor
public class SubscriptionController {

    private final PlanRepository repository;

    @GetMapping
    public List<Plan> getPlans() {

        return repository.findAll();

    }

}