package com.IntegrityTechnologies.business_manager.modules.platform.tenant.controller;

import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.PlatformAdminOnly;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.dto.TenantCreateRequest;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.dto.TenantResponse;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.service.TenantService;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.web.bind.annotation.*;

import java.util.UUID;

@Tag(name = "Tenants")
@RestController
@RequestMapping("/api/platform/tenants")
@RequiredArgsConstructor
@PlatformAdminOnly
public class TenantController {

    private final TenantService tenantService;

    /* ==========================================
       CREATE TENANT
    ========================================== */

    @PostMapping
    public TenantResponse createTenant(
            @RequestBody TenantCreateRequest request
    ) {
        return tenantService.createTenant(request);
    }

    /* ==========================================
       GET TENANT
    ========================================== */

    @GetMapping("/{id}")
    public TenantResponse getTenant(
            @PathVariable UUID id
    ) {
        return tenantService.getTenant(id);
    }

    /* ==========================================
       PAGINATED TENANTS
    ========================================== */

    @GetMapping
    public Page<TenantResponse> getTenants(
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "20") int size
    ) {
        return tenantService.getTenants(page, size);
    }

}