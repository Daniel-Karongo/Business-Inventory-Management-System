package com.IntegrityTechnologies.business_manager.modules.platform.subscription.controller;

import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.PlatformAdminOnly;
import com.IntegrityTechnologies.business_manager.modules.platform.subscription.dto.TenantSubscriptionResponse;
import com.IntegrityTechnologies.business_manager.modules.platform.subscription.service.TenantSubscriptionQueryService;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.service.TenantAdminService;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

import java.util.UUID;

@RestController
@RequestMapping("/api/platform/subscriptions")
@RequiredArgsConstructor
@PlatformAdminOnly
public class TenantSubscriptionController {

    private final TenantSubscriptionQueryService queryService;

    private final TenantAdminService tenantAdminService;

    @GetMapping("/{tenantId}")
    public TenantSubscriptionResponse getSubscription(
            @PathVariable UUID tenantId
    ) {

        return queryService.getSubscription(tenantId);

    }

    @PostMapping("/{tenantId}/assign")
    public void assignPlan(
            @PathVariable UUID tenantId,
            @RequestBody AssignPlanRequest request
    ) {

        tenantAdminService.changePlan(
                tenantId,
                request.planCode()
        );

    }

    public record AssignPlanRequest(String planCode) {}

}