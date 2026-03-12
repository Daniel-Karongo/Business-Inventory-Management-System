package com.IntegrityTechnologies.business_manager.modules.platform.settings.controller;

import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.PlatformUserOrTenantManager;
import com.IntegrityTechnologies.business_manager.modules.platform.settings.service.TenantLogoService;
import com.IntegrityTechnologies.business_manager.modules.platform.settings.service.TenantSettingsService;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.entity.TenantSettings;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.core.io.Resource;
import org.springframework.core.io.UrlResource;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import java.nio.file.Path;

@Tag(name = "Tenant Settings")
@RestController
@RequestMapping("/api/platform/settings")
@RequiredArgsConstructor
public class TenantSettingsController {

    private final TenantSettingsService service;
    private final TenantLogoService logoService;

    @GetMapping
    @PlatformUserOrTenantManager
    public TenantSettings getSettings() {
        return service.getSettings();

    }

    @PostMapping("/logo")
    @PlatformUserOrTenantManager
    public String uploadLogo(
            @RequestParam("file") MultipartFile file
    ) throws Exception {

        return logoService.uploadLogo(file);

    }

    @GetMapping("/logo")
    public ResponseEntity<Resource> logo() throws Exception {

        TenantSettings settings = service.getSettings();

        if (settings.getLogoUrl() == null) {
            return ResponseEntity.notFound().build();
        }

        Path path =
                logoService.resolveLogo(settings.getLogoUrl());

        Resource resource =
                new UrlResource(path.toUri());

        return ResponseEntity.ok()
                .contentType(MediaType.IMAGE_PNG)
                .header("Cache-Control", "public, max-age=3600")
                .body(resource);
    }
}