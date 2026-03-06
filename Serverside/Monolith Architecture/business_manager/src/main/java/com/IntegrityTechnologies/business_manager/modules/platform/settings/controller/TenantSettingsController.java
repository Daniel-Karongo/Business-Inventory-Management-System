package com.IntegrityTechnologies.business_manager.modules.platform.settings.controller;

import com.IntegrityTechnologies.business_manager.modules.platform.settings.service.TenantSettingsService;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.entity.TenantSettings;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/platform/settings")
@RequiredArgsConstructor
public class TenantSettingsController {

    private final TenantSettingsService service;

    @GetMapping
    public TenantSettings getSettings() {
        return service.getSettings();
    }

}