package com.IntegrityTechnologies.business_manager.security.acl.controller;

import com.IntegrityTechnologies.business_manager.security.acl.service.PermissionCacheService;
import lombok.RequiredArgsConstructor;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/admin/acl/cache")
@RequiredArgsConstructor
public class PermissionCacheController {

    private final PermissionCacheService cache;

    @PostMapping("/refresh")
    @PreAuthorize("hasRole('SUPERUSER')")
    public void refresh() {
        cache.refresh();
    }
}