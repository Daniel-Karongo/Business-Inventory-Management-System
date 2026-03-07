package com.IntegrityTechnologies.business_manager.modules.platform.tenant.controller;

import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.PlatformAdminOnly;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.service.TenantAdminService;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

import java.util.UUID;

@Tag(name = "Platform Tenant Admin")
@RestController
@RequestMapping("/api/platform/admin/tenants")
@RequiredArgsConstructor
@PlatformAdminOnly
public class TenantAdminController {

    private final TenantAdminService tenantAdminService;

    @PostMapping("/{tenantId}/suspend")
    public void suspend(
            @PathVariable UUID tenantId
    ) {

        tenantAdminService.suspendTenant(tenantId);

    }

    @PostMapping("/{tenantId}/activate")
    public void activate(
            @PathVariable UUID tenantId
    ) {

        tenantAdminService.activateTenant(tenantId);

    }

    @PostMapping("/{tenantId}/expire")
    public void expire(
            @PathVariable UUID tenantId
    ) {

        tenantAdminService.expireTenant(tenantId);

    }

    @PostMapping("/{tenantId}/plan/{planCode}")
    public void changePlan(
            @PathVariable UUID tenantId,
            @PathVariable String planCode
    ) {

        tenantAdminService.changePlan(
                tenantId,
                planCode
        );

    }

}