package com.IntegrityTechnologies.business_manager.modules.platform.settings.controller;

import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantAdminOnly;
import com.IntegrityTechnologies.business_manager.modules.platform.settings.service.TenantSettingsService;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.entity.TenantSettings;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

@Tag(name = "Tenant Settings")
@RestController
@RequestMapping("/api/platform/settings")
@RequiredArgsConstructor
@TenantAdminOnly
public class TenantSettingsController {

    private final TenantSettingsService service;

    @GetMapping
    public TenantSettings getSettings() {

        return service.getSettings();

    }

}